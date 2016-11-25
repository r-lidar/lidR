/*
===============================================================================

  FILE:  laswriter_las.cpp

  CONTENTS:

    see corresponding header file

  PROGRAMMERS:

    martin.isenburg@rapidlasso.com  -  http://rapidlasso.com

  COPYRIGHT:

    (c) 2007-2012, martin isenburg, rapidlasso - fast tools to catch reality

    This is free software; you can redistribute and/or modify it under the
    terms of the GNU Lesser General Licence as published by the Free Software
    Foundation. See the LICENSE.txt file for more information.

    This software is distributed WITHOUT ANY WARRANTY and without even the
    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  CHANGE HISTORY:

    see corresponding header file

===============================================================================
*/
#include "laswriter_las.hpp"

#include "bytestreamout_nil.hpp"
#include "bytestreamout_file.hpp"
#include "bytestreamout_ostream.hpp"
#include "laswritepoint.hpp"

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif


#include <Rcpp.h>

BOOL LASwriterLAS::refile(FILE* file)
{
  if (stream == 0) return FALSE;
  if (this->file) this->file = file;
  return ((ByteStreamOutFile*)stream)->refile(file);
}

BOOL LASwriterLAS::open(const LASheader* header, U32 compressor, I32 requested_version, I32 chunk_size)
{
  ByteStreamOut* out = new ByteStreamOutNil();
  return open(out, header, compressor, requested_version, chunk_size);
}

BOOL LASwriterLAS::open(const char* file_name, const LASheader* header, U32 compressor, I32 requested_version, I32 chunk_size, I32 io_buffer_size)
{
  if (file_name == 0)
  {
    Rcpp::Rcerr << "ERROR: file name pointer is zero" << std::endl;
    return FALSE;
  }

  file = fopen(file_name, "wb");
  if (file == 0)
  {
    Rcpp::Rcerr << "ERROR: cannot open file '" << file_name << "'" << std::endl;
    return FALSE;
  }

  if (setvbuf(file, NULL, _IOFBF, io_buffer_size) != 0)
  {
    Rcpp::Rcerr << "WARNING: setvbuf() failed with buffer size " << io_buffer_size << "" << std::endl;
  }

  ByteStreamOut* out;
  if (IS_LITTLE_ENDIAN())
    out = new ByteStreamOutFileLE(file);
  else
    out = new ByteStreamOutFileBE(file);

  return open(out, header, compressor, requested_version, chunk_size);
}

BOOL LASwriterLAS::open(FILE* file, const LASheader* header, U32 compressor, I32 requested_version, I32 chunk_size)
{
  if (file == 0)
  {
    Rcpp::Rcerr << "ERROR: file pointer is zero" << std::endl;
    return FALSE;
  }

// JR : remove stdoutput
/*#ifdef _WIN32
  if (file == stdoutput)
  {
    if(_setmode( _fileno( stdoutput ), _O_BINARY ) == -1 )
    {
      Rcpp::Rcerr << "ERROR: cannot set stdoutput to binary (untranslated) mode" << std::endl;
    }
  }
#endif*/

  ByteStreamOut* out;
  if (IS_LITTLE_ENDIAN())
    out = new ByteStreamOutFileLE(file);
  else
    out = new ByteStreamOutFileBE(file);

  return open(out, header, compressor, requested_version, chunk_size);
}

BOOL LASwriterLAS::open(ostream& stream, const LASheader* header, U32 compressor, I32 requested_version, I32 chunk_size)
{
  ByteStreamOut* out;
  if (IS_LITTLE_ENDIAN())
    out = new ByteStreamOutOstreamLE(stream);
  else
    out = new ByteStreamOutOstreamBE(stream);

  return open(out, header, compressor, requested_version, chunk_size);
}

BOOL LASwriterLAS::open(ByteStreamOut* stream, const LASheader* header, U32 compressor, I32 requested_version, I32 chunk_size)
{
  U32 i, j;

  if (stream == 0)
  {
    Rcpp::Rcerr << "ERROR: ByteStreamOut pointer is zero" << std::endl;
    return FALSE;
  }
  this->stream = stream;

  if (header == 0)
  {
    Rcpp::Rcerr << "ERROR: LASheader pointer is zero" << std::endl;
    return FALSE;
  }

  // check header contents

  if (!header->check()) return FALSE;

  // copy scale_and_offset
  quantizer.x_scale_factor = header->x_scale_factor;
  quantizer.y_scale_factor = header->y_scale_factor;
  quantizer.z_scale_factor = header->z_scale_factor;
  quantizer.x_offset = header->x_offset;
  quantizer.y_offset = header->y_offset;
  quantizer.z_offset = header->z_offset;

  // check if the requested point type is supported

  LASpoint point;
  U8 point_data_format;
  U16 point_data_record_length;
  BOOL point_is_standard = TRUE;

  if (header->laszip)
  {
    if (!point.init(&quantizer, header->laszip->num_items, header->laszip->items, header)) return FALSE;
    point_is_standard = header->laszip->is_standard(&point_data_format, &point_data_record_length);
  }
  else
  {
    if (!point.init(&quantizer, header->point_data_format, header->point_data_record_length, header)) return FALSE;
    point_data_format = header->point_data_format;
    point_data_record_length = header->point_data_record_length;
  }

  // do we need a LASzip VLR (because we compress or use non-standard points?)

  LASzip* laszip = 0;
  U32 laszip_vlr_data_size = 0;
  if (compressor || point_is_standard == FALSE)
  {
    laszip = new LASzip();
    laszip->setup(point.num_items, point.items, compressor);
    if (chunk_size > -1) laszip->set_chunk_size((U32)chunk_size);
    if (compressor == LASZIP_COMPRESSOR_NONE) laszip->request_version(0);
    else if (chunk_size == 0) { Rcpp::Rcerr << "ERROR: adaptive chunking is depricated" << std::endl; return FALSE; }
    else if (requested_version) laszip->request_version(requested_version);
    else laszip->request_version(2);
    laszip_vlr_data_size = 34 + 6*laszip->num_items;
  }

  // create and setup the point writer

  writer = new LASwritePoint();
  if (laszip)
  {
    if (!writer->setup(laszip->num_items, laszip->items, laszip))
    {
      Rcpp::Rcerr << "ERROR: point type " << header->point_data_format << " of size " << header->point_data_record_length << " not supported (with LASzip)" << std::endl;
      return FALSE;
    }
  }
  else
  {
    if (!writer->setup(point.num_items, point.items))
    {
      Rcpp::Rcerr << "ERROR: point type " << header->point_data_format << " of size " << header->point_data_record_length << " not supported" << std::endl;
      return FALSE;
    }
  }

  // save the position where we start writing the header

  header_start_position = stream->tell();

  // write header variable after variable (to avoid alignment issues)

  if (!stream->putBytes((U8*)&(header->file_signature), 4))
  {
    Rcpp::Rcerr << "ERROR: writing header->file_signature" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->file_source_ID)))
  {
    Rcpp::Rcerr << "ERROR: writing header->file_source_ID" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->global_encoding)))
  {
    Rcpp::Rcerr << "ERROR: writing header->global_encoding" << std::endl;
    return FALSE;
  }
  if (!stream->put32bitsLE((U8*)&(header->project_ID_GUID_data_1)))
  {
    Rcpp::Rcerr << "ERROR: writing header->project_ID_GUID_data_1" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->project_ID_GUID_data_2)))
  {
    Rcpp::Rcerr << "ERROR: writing header->project_ID_GUID_data_2" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->project_ID_GUID_data_3)))
  {
    Rcpp::Rcerr << "ERROR: writing header->project_ID_GUID_data_3" << std::endl;
    return FALSE;
  }
  if (!stream->putBytes((U8*)header->project_ID_GUID_data_4, 8))
  {
    Rcpp::Rcerr << "ERROR: writing header->project_ID_GUID_data_4" << std::endl;
    return FALSE;
  }
  // check version major
  U8 version_major = header->version_major;
  if (header->version_major != 1)
  {
    Rcpp::Rcerr << "WARNING: header->version_major is " << header->version_major << ". writing 1 instead." << std::endl;
    version_major = 1;
  }
  if (!stream->putByte(header->version_major))
  {
    Rcpp::Rcerr << "ERROR: writing header->version_major" << std::endl;
    return FALSE;
  }
  // check version minor
  U8 version_minor = header->version_minor;
  if (version_minor > 4)
  {
    Rcpp::Rcerr << "WARNING: header->version_minor is " << version_minor << ". writing 4 instead." << std::endl;
    version_minor = 4;
  }
  if (!stream->putByte(version_minor))
  {
    Rcpp::Rcerr << "ERROR: writing header->version_minor" << std::endl;
    return FALSE;
  }
  if (!stream->putBytes((U8*)header->system_identifier, 32))
  {
    Rcpp::Rcerr << "ERROR: writing header->system_identifier" << std::endl;
    return FALSE;
  }
  if (!stream->putBytes((U8*)header->generating_software, 32))
  {
    Rcpp::Rcerr << "ERROR: writing header->generating_software" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->file_creation_day)))
  {
    Rcpp::Rcerr << "ERROR: writing header->file_creation_day" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->file_creation_year)))
  {
    Rcpp::Rcerr << "ERROR: writing header->file_creation_year" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->header_size)))
  {
    Rcpp::Rcerr << "ERROR: writing header->header_size" << std::endl;
    return FALSE;
  }
  U32 offset_to_point_data = header->offset_to_point_data;
  if (laszip) offset_to_point_data += (54 + laszip_vlr_data_size);
  if (header->vlr_lastiling) offset_to_point_data += (54 + 28);
  if (header->vlr_lasoriginal) offset_to_point_data += (54 + 176);
  if (!stream->put32bitsLE((U8*)&offset_to_point_data))
  {
    Rcpp::Rcerr << "ERROR: writing header->offset_to_point_data" << std::endl;
    return FALSE;
  }
  U32 number_of_variable_length_records = header->number_of_variable_length_records;
  if (laszip) number_of_variable_length_records++;
  if (header->vlr_lastiling) number_of_variable_length_records++;
  if (header->vlr_lasoriginal) number_of_variable_length_records++;
  if (!stream->put32bitsLE((U8*)&(number_of_variable_length_records)))
  {
    Rcpp::Rcerr << "ERROR: writing header->number_of_variable_length_records" << std::endl;
    return FALSE;
  }
  if (compressor) point_data_format |= 128;
  if (!stream->putByte(point_data_format))
  {
    Rcpp::Rcerr << "ERROR: writing header->point_data_format" << std::endl;
    return FALSE;
  }
  if (!stream->put16bitsLE((U8*)&(header->point_data_record_length)))
  {
    Rcpp::Rcerr << "ERROR: writing header->point_data_record_length" << std::endl;
    return FALSE;
  }
  if (!stream->put32bitsLE((U8*)&(header->number_of_point_records)))
  {
    Rcpp::Rcerr << "ERROR: writing header->number_of_point_records" << std::endl;
    return FALSE;
  }
  for (i = 0; i < 5; i++)
  {
    if (!stream->put32bitsLE((U8*)&(header->number_of_points_by_return[i])))
    {
      Rcpp::Rcerr << "ERROR: writing header->number_of_points_by_return[" << i << "]" << std::endl;
      return FALSE;
    }
  }
  if (!stream->put64bitsLE((U8*)&(header->x_scale_factor)))
  {
    Rcpp::Rcerr << "ERROR: writing header->x_scale_factor" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->y_scale_factor)))
  {
    Rcpp::Rcerr << "ERROR: writing header->y_scale_factor" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->z_scale_factor)))
  {
    Rcpp::Rcerr << "ERROR: writing header->z_scale_factor" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->x_offset)))
  {
    Rcpp::Rcerr << "ERROR: writing header->x_offset" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->y_offset)))
  {
    Rcpp::Rcerr << "ERROR: writing header->y_offset" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->z_offset)))
  {
    Rcpp::Rcerr << "ERROR: writing header->z_offset" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->max_x)))
  {
    Rcpp::Rcerr << "ERROR: writing header->max_x" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->min_x)))
  {
    Rcpp::Rcerr << "ERROR: writing header->min_x" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->max_y)))
  {
    Rcpp::Rcerr << "ERROR: writing header->max_y" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->min_y)))
  {
    Rcpp::Rcerr << "ERROR: writing header->min_y" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->max_z)))
  {
    Rcpp::Rcerr << "ERROR: writing header->max_z" << std::endl;
    return FALSE;
  }
  if (!stream->put64bitsLE((U8*)&(header->min_z)))
  {
    Rcpp::Rcerr << "ERROR: writing header->min_z" << std::endl;
    return FALSE;
  }

  // special handling for LAS 1.3 or higher.
  if (version_minor >= 3)
  {
    U64 start_of_waveform_data_packet_record = header->start_of_waveform_data_packet_record;
    if (start_of_waveform_data_packet_record != 0)
    {
#ifdef _WIN32
      Rcpp::Rcerr << "WARNING: header->start_of_waveform_data_packet_record is " << start_of_waveform_data_packet_record << ". writing 0 instead." << std::endl;
#else
      Rcpp::Rcerr << "WARNING: header->start_of_waveform_data_packet_record is " << start_of_waveform_data_packet_record << ". writing 0 instead." << std::endl;
#endif
      start_of_waveform_data_packet_record = 0;
    }
    if (!stream->put64bitsLE((U8*)&start_of_waveform_data_packet_record))
    {
      Rcpp::Rcerr << "ERROR: writing start_of_waveform_data_packet_record" << std::endl;
      return FALSE;
    }
  }

  // special handling for LAS 1.4 or higher.
  if (version_minor >= 4)
  {
    writing_las_1_4 = TRUE;
    if (header->point_data_format >= 6)
    {
      writing_new_point_type = TRUE;
    }
    else
    {
      writing_new_point_type = FALSE;
    }

    U64 start_of_first_extended_variable_length_record = header->start_of_first_extended_variable_length_record;
    if (start_of_first_extended_variable_length_record != 0)
    {
#ifdef _WIN32
      Rcpp::Rcerr << "WARNING: EVLRs not supported. header->start_of_first_extended_variable_length_record is " << start_of_first_extended_variable_length_record << ". writing 0 instead." << std::endl;
#else
      Rcpp::Rcerr << "WARNING: EVLRs not supported. header->start_of_first_extended_variable_length_record is " << start_of_first_extended_variable_length_record << ". writing 0 instead." << std::endl;
#endif
      start_of_first_extended_variable_length_record = 0;
    }
    if (!stream->put64bitsLE((U8*)&(start_of_first_extended_variable_length_record)))
    {
      Rcpp::Rcerr << "ERROR: writing header->start_of_first_extended_variable_length_record" << std::endl;
      return FALSE;
    }
    U32 number_of_extended_variable_length_records = header->number_of_extended_variable_length_records;
    if (number_of_extended_variable_length_records != 0)
    {
      Rcpp::Rcerr << "WARNING: EVLRs not supported. header->number_of_extended_variable_length_records is " << number_of_extended_variable_length_records << ". writing 0 instead." << std::endl;
      number_of_extended_variable_length_records = 0;
    }
    if (!stream->put32bitsLE((U8*)&(number_of_extended_variable_length_records)))
    {
      Rcpp::Rcerr << "ERROR: writing header->number_of_extended_variable_length_records" << std::endl;
      return FALSE;
    }
    U64 extended_number_of_point_records;
    if (header->number_of_point_records)
      extended_number_of_point_records = header->number_of_point_records;
    else
      extended_number_of_point_records = header->extended_number_of_point_records;
    if (!stream->put64bitsLE((U8*)&extended_number_of_point_records))
    {
      Rcpp::Rcerr << "ERROR: writing header->extended_number_of_point_records" << std::endl;
      return FALSE;
    }
    U64 extended_number_of_points_by_return;
    for (i = 0; i < 15; i++)
    {
      if ((i < 5) && header->number_of_points_by_return[i])
        extended_number_of_points_by_return = header->number_of_points_by_return[i];
      else
        extended_number_of_points_by_return = header->extended_number_of_points_by_return[i];
      if (!stream->put64bitsLE((U8*)&extended_number_of_points_by_return))
      {
        Rcpp::Rcerr << "ERROR: writing header->extended_number_of_points_by_return[" << i << "]" << std::endl;
        return FALSE;
      }
    }
  }
  else
  {
    writing_las_1_4 = FALSE;
    writing_new_point_type = FALSE;
  }

  // write any number of user-defined bytes that might have been added into the header

  if (header->user_data_in_header_size)
  {
    if (header->user_data_in_header)
    {
      if (!stream->putBytes((U8*)header->user_data_in_header, header->user_data_in_header_size))
      {
        Rcpp::Rcerr << "ERROR: writing " << header->user_data_in_header_size << " bytes of data from header->user_data_in_header" << std::endl;
        return FALSE;
      }
    }
    else
    {
      Rcpp::Rcerr << "ERROR: there should be " << header->user_data_in_header_size << " bytes of data in header->user_data_in_header" << std::endl;
      return FALSE;
    }
  }

  // write variable length records variable after variable (to avoid alignment issues)

  for (i = 0; i < header->number_of_variable_length_records; i++)
  {
    // check variable length records contents

    if (header->vlrs[i].reserved != 0xAABB)
    {
//      Rcpp::Rcerr << "WARNING: wrong header->vlrs[" << i << "].reserved: " << header->vlrs[i].reserved << " != 0xAABB" << std::endl;
    }

    // write variable length records variable after variable (to avoid alignment issues)

    if (!stream->put16bitsLE((U8*)&(header->vlrs[i].reserved)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlrs[" << i << "].reserved" << std::endl;
      return FALSE;
    }
    if (!stream->putBytes((U8*)header->vlrs[i].user_id, 16))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlrs[" << i << "].user_id" << std::endl;
      return FALSE;
    }
    if (!stream->put16bitsLE((U8*)&(header->vlrs[i].record_id)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlrs[" << i << "].record_id" << std::endl;
      return FALSE;
    }
    if (!stream->put16bitsLE((U8*)&(header->vlrs[i].record_length_after_header)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlrs[" << i << "].record_length_after_header" << std::endl;
      return FALSE;
    }
    if (!stream->putBytes((U8*)header->vlrs[i].description, 32))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlrs[" << i << "].description" << std::endl;
      return FALSE;
    }

    // write the data following the header of the variable length record

    if (header->vlrs[i].record_length_after_header)
    {
      if (header->vlrs[i].data)
      {
        if (!stream->putBytes((U8*)header->vlrs[i].data, header->vlrs[i].record_length_after_header))
        {
          Rcpp::Rcerr << "ERROR: writing " << header->vlrs[i].record_length_after_header << " bytes of data from header->vlrs[" << i << "].data" << std::endl;
          return FALSE;
        }
      }
      else
      {
        Rcpp::Rcerr << "ERROR: there should be " << header->vlrs[i].record_length_after_header << " bytes of data in header->vlrs[" << i << "].data" << std::endl;
        return FALSE;
      }
    }
  }

  // write laszip VLR with compression parameters

  if (laszip)
  {
    // write variable length records variable after variable (to avoid alignment issues)

    U16 reserved = 0xAABB;
    if (!stream->put16bitsLE((U8*)&(reserved)))
    {
      Rcpp::Rcerr << "ERROR: writing reserved " << (I32)reserved << "" << std::endl;
      return FALSE;
    }
    U8 user_id[16] = "laszip encoded\0";
    if (!stream->putBytes((U8*)user_id, 16))
    {
      Rcpp::Rcerr << "ERROR: writing user_id " << user_id << "" << std::endl;
      return FALSE;
    }
    U16 record_id = 22204;
    if (!stream->put16bitsLE((U8*)&(record_id)))
    {
      Rcpp::Rcerr << "ERROR: writing record_id " << (I32)record_id << "" << std::endl;
      return FALSE;
    }
    U16 record_length_after_header = laszip_vlr_data_size;
    if (!stream->put16bitsLE((U8*)&(record_length_after_header)))
    {
      Rcpp::Rcerr << "ERROR: writing record_length_after_header " << (I32)record_length_after_header << "" << std::endl;
      return FALSE;
    }
    char description[32];
    memset(description, 0, 32);
    sprintf(description, "by laszip of LAStools (%d)", LAS_TOOLS_VERSION);
    if (!stream->putBytes((U8*)description, 32))
    {
      Rcpp::Rcerr << "ERROR: writing description " << description << "" << std::endl;
      return FALSE;
    }
    // write the data following the header of the variable length record
    //     U16  compressor                2 bytes
    //     U32  coder                     2 bytes
    //     U8   version_major             1 byte
    //     U8   version_minor             1 byte
    //     U16  version_revision          2 bytes
    //     U32  options                   4 bytes
    //     I32  chunk_size                4 bytes
    //     I64  number_of_special_evlrs   8 bytes
    //     I64  offset_to_special_evlrs   8 bytes
    //     U16  num_items                 2 bytes
    //        U16 type                2 bytes * num_items
    //        U16 size                2 bytes * num_items
    //        U16 version             2 bytes * num_items
    // which totals 34+6*num_items

    if (!stream->put16bitsLE((U8*)&(laszip->compressor)))
    {
      Rcpp::Rcerr << "ERROR: writing compressor " << (I32)compressor << "" << std::endl;
      return FALSE;
    }
    if (!stream->put16bitsLE((U8*)&(laszip->coder)))
    {
      Rcpp::Rcerr << "ERROR: writing coder " << (I32)laszip->coder << "" << std::endl;
      return FALSE;
    }
    if (!stream->putByte(laszip->version_major))
    {
      Rcpp::Rcerr << "ERROR: writing version_major " << laszip->version_major << "" << std::endl;
      return FALSE;
    }
    if (!stream->putByte(laszip->version_minor))
    {
      Rcpp::Rcerr << "ERROR: writing version_minor " << laszip->version_minor << "" << std::endl;
      return FALSE;
    }
    if (!stream->put16bitsLE((U8*)&(laszip->version_revision)))
    {
      Rcpp::Rcerr << "ERROR: writing version_revision " << laszip->version_revision << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(laszip->options)))
    {
      Rcpp::Rcerr << "ERROR: writing options " << (I32)laszip->options << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(laszip->chunk_size)))
    {
      Rcpp::Rcerr << "ERROR: writing chunk_size " << laszip->chunk_size << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(laszip->number_of_special_evlrs)))
    {
      Rcpp::Rcerr << "ERROR: writing number_of_special_evlrs " << (I32)laszip->number_of_special_evlrs << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(laszip->offset_to_special_evlrs)))
    {
      Rcpp::Rcerr << "ERROR: writing offset_to_special_evlrs " << (I32)laszip->offset_to_special_evlrs << "" << std::endl;
      return FALSE;
    }
    if (!stream->put16bitsLE((U8*)&(laszip->num_items)))
    {
      Rcpp::Rcerr << "ERROR: writing num_items " << laszip->num_items << "" << std::endl;
      return FALSE;
    }
    for (i = 0; i < laszip->num_items; i++)
    {
      if (!stream->put16bitsLE((U8*)&(laszip->items[i].type)))
      {
        Rcpp::Rcerr << "ERROR: writing type " << laszip->items[i].type << " of item " << i << "" << std::endl;
        return FALSE;
      }
      if (!stream->put16bitsLE((U8*)&(laszip->items[i].size)))
      {
        Rcpp::Rcerr << "ERROR: writing size " << laszip->items[i].size << " of item " << i << "" << std::endl;
        return FALSE;
      }
      if (!stream->put16bitsLE((U8*)&(laszip->items[i].version)))
      {
        Rcpp::Rcerr << "ERROR: writing version " << laszip->items[i].version << " of item " << i << "" << std::endl;
        return FALSE;
      }
    }

    delete laszip;
    laszip = 0;
  }

  // write lastiling VLR with the tile parameters

  if (header->vlr_lastiling)
  {
    // write variable length records variable after variable (to avoid alignment issues)

    U16 reserved = 0xAABB;
    if (!stream->put16bitsLE((U8*)&(reserved)))
    {
      Rcpp::Rcerr << "ERROR: writing reserved " << (I32)reserved << "" << std::endl;
      return FALSE;
    }
    U8 user_id[16] = "LAStools\0\0\0\0\0\0\0";
    if (!stream->putBytes((U8*)user_id, 16))
    {
      Rcpp::Rcerr << "ERROR: writing user_id " << user_id << "" << std::endl;
      return FALSE;
    }
    U16 record_id = 10;
    if (!stream->put16bitsLE((U8*)&(record_id)))
    {
      Rcpp::Rcerr << "ERROR: writing record_id " << (I32)record_id << "" << std::endl;
      return FALSE;
    }
    U16 record_length_after_header = 28;
    if (!stream->put16bitsLE((U8*)&(record_length_after_header)))
    {
      Rcpp::Rcerr << "ERROR: writing record_length_after_header " << (I32)record_length_after_header << "" << std::endl;
      return FALSE;
    }
    CHAR description[32] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
    sprintf(description, "tile %s buffer %s", (header->vlr_lastiling->buffer ? "with" : "without"), (header->vlr_lastiling->reversible ? ", reversible" : ""));
    if (!stream->putBytes((U8*)description, 32))
    {
      Rcpp::Rcerr << "ERROR: writing description " << description << "" << std::endl;
      return FALSE;
    }

    // write the payload of this VLR which contains 28 bytes
    //   U32  level                                          4 bytes
    //   U32  level_index                                    4 bytes
    //   U32  implicit_levels + buffer bit + reversible bit  4 bytes
    //   F32  min_x                                          4 bytes
    //   F32  max_x                                          4 bytes
    //   F32  min_y                                          4 bytes
    //   F32  max_y                                          4 bytes

    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->level)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->level " << header->vlr_lastiling->level << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->level_index)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->level_index " << header->vlr_lastiling->level_index << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE(((U8*)header->vlr_lastiling)+8))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->implicit_levels " << header->vlr_lastiling->implicit_levels << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->min_x)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->min_x " << header->vlr_lastiling->min_x << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->max_x)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->max_x " << header->vlr_lastiling->max_x << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->min_y)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->min_y " << header->vlr_lastiling->min_y << "" << std::endl;
      return FALSE;
    }
    if (!stream->put32bitsLE((U8*)&(header->vlr_lastiling->max_y)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lastiling->max_y " << header->vlr_lastiling->max_y << "" << std::endl;
      return FALSE;
    }
  }

  // write lasoriginal VLR with the original (unbuffered) counts and bounding box extent

  if (header->vlr_lasoriginal)
  {
    // write variable length records variable after variable (to avoid alignment issues)

    U16 reserved = 0xAABB;
    if (!stream->put16bitsLE((U8*)&(reserved)))
    {
      Rcpp::Rcerr << "ERROR: writing reserved " << (I32)reserved << "" << std::endl;
      return FALSE;
    }
    U8 user_id[16] = "LAStools\0\0\0\0\0\0\0";
    if (!stream->putBytes((U8*)user_id, 16))
    {
      Rcpp::Rcerr << "ERROR: writing user_id " << user_id << "" << std::endl;
      return FALSE;
    }
    U16 record_id = 20;
    if (!stream->put16bitsLE((U8*)&(record_id)))
    {
      Rcpp::Rcerr << "ERROR: writing record_id " << (I32)record_id << "" << std::endl;
      return FALSE;
    }
    U16 record_length_after_header = 176;
    if (!stream->put16bitsLE((U8*)&(record_length_after_header)))
    {
      Rcpp::Rcerr << "ERROR: writing record_length_after_header " << (I32)record_length_after_header << "" << std::endl;
      return FALSE;
    }
    U8 description[32] = "counters and bbox of original\0\0";
    if (!stream->putBytes((U8*)description, 32))
    {
      Rcpp::Rcerr << "ERROR: writing description " << description << "" << std::endl;
      return FALSE;
    }

    // write the payload of this VLR which contains 176 bytes

    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->number_of_point_records)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->number_of_point_records " << (U32)header->vlr_lasoriginal->number_of_point_records << "" << std::endl;
      return FALSE;
    }
    for (j = 0; j < 15; j++)
    {
      if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->number_of_points_by_return[j])))
      {
        Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->number_of_points_by_return[" << j << "] " << (U32)header->vlr_lasoriginal->number_of_points_by_return[j] << "" << std::endl;
        return FALSE;
      }
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->min_x)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->min_x " << header->vlr_lasoriginal->min_x << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->max_x)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->max_x " << header->vlr_lasoriginal->max_x << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->min_y)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->min_y " << header->vlr_lasoriginal->min_y << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->max_y)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->max_y " << header->vlr_lasoriginal->max_y << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->min_z)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->min_z " << header->vlr_lasoriginal->min_z << "" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->vlr_lasoriginal->max_z)))
    {
      Rcpp::Rcerr << "ERROR: writing header->vlr_lasoriginal->max_z " << header->vlr_lasoriginal->max_z << "" << std::endl;
      return FALSE;
    }
  }

  // write any number of user-defined bytes that might have been added after the header

  if (header->user_data_after_header_size)
  {
    if (header->user_data_after_header)
    {
      if (!stream->putBytes((U8*)header->user_data_after_header, header->user_data_after_header_size))
      {
        Rcpp::Rcerr << "ERROR: writing " << header->user_data_after_header_size << " bytes of data from header->user_data_after_header" << std::endl;
        return FALSE;
      }
    }
    else
    {
      Rcpp::Rcerr << "ERROR: there should be " << header->user_data_after_header_size << " bytes of data in header->user_data_after_header" << std::endl;
      return FALSE;
    }
  }

  // initialize the point writer

  if (!writer->init(stream)) return FALSE;

  npoints = (header->number_of_point_records ? header->number_of_point_records : header->extended_number_of_point_records);
  p_count = 0;

  return TRUE;
}

BOOL LASwriterLAS::write_point(const LASpoint* point)
{
  p_count++;
  return writer->write(point->point);
}

BOOL LASwriterLAS::chunk()
{
  return writer->chunk();
}

BOOL LASwriterLAS::update_header(const LASheader* header, BOOL use_inventory, BOOL update_extra_bytes)
{
  I32 i;
  if (header == 0)
  {
    Rcpp::Rcerr << "ERROR: header pointer is zero" << std::endl;
    return FALSE;
  }
  if (stream == 0)
  {
    Rcpp::Rcerr << "ERROR: stream pointer is zero" << std::endl;
    return FALSE;
  }
  if (!stream->isSeekable())
  {
    Rcpp::Rcerr << "WARNING: stream not seekable. cannot update header." << std::endl;
    return FALSE;
  }
  if (use_inventory)
  {
    U32 number;
    stream->seek(header_start_position+107);
    if (header->point_data_format >= 6)
    {
      number = 0; // legacy counters are zero for new point types
    }
    else if (inventory.extended_number_of_point_records > U32_MAX)
    {
      if (header->version_minor >= 4)
      {
        number = 0;
      }
      else
      {
        Rcpp::Rcerr << "WARNING: too many points in LAS " << header->version_major << "." << header->version_minor << " file. limit is " << U32_MAX << "." << std::endl;
        number = U32_MAX;
      }
    }
    else
    {
      number = (U32)inventory.extended_number_of_point_records;
    }
    if (!stream->put32bitsLE((U8*)&number))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.number_of_point_records" << std::endl;
      return FALSE;
    }
    npoints = inventory.extended_number_of_point_records;
    for (i = 0; i < 5; i++)
    {
      if (header->point_data_format >= 6)
      {
        number = 0; // legacy counters are zero for new point types
      }
      else if (inventory.extended_number_of_points_by_return[i+1] > U32_MAX)
      {
        if (header->version_minor >= 4)
        {
          number = 0;
        }
        else
        {
          number = U32_MAX;
        }
      }
      else
      {
        number = (U32)inventory.extended_number_of_points_by_return[i+1];
      }
      if (!stream->put32bitsLE((U8*)&number))
      {
        Rcpp::Rcerr << "ERROR: updating inventory.number_of_points_by_return[" << i << "]" << std::endl;
        return FALSE;
      }
    }
    stream->seek(header_start_position+179);
    F64 value;
    value = quantizer.get_x(inventory.max_X);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.max_X" << std::endl;
      return FALSE;
    }
    value = quantizer.get_x(inventory.min_X);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.min_X" << std::endl;
      return FALSE;
    }
    value = quantizer.get_y(inventory.max_Y);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.max_Y" << std::endl;
      return FALSE;
    }
    value = quantizer.get_y(inventory.min_Y);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.min_Y" << std::endl;
      return FALSE;
    }
    value = quantizer.get_z(inventory.max_Z);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.max_Z" << std::endl;
      return FALSE;
    }
    value = quantizer.get_z(inventory.min_Z);
    if (!stream->put64bitsLE((U8*)&value))
    {
      Rcpp::Rcerr << "ERROR: updating inventory.min_Z" << std::endl;
      return FALSE;
    }
    // special handling for LAS 1.4 or higher.
    if (header->version_minor >= 4)
    {
      stream->seek(header_start_position+247);
      if (!stream->put64bitsLE((U8*)&(inventory.extended_number_of_point_records)))
      {
        Rcpp::Rcerr << "ERROR: updating header->extended_number_of_point_records" << std::endl;
        return FALSE;
      }
      for (i = 0; i < 15; i++)
      {
        if (!stream->put64bitsLE((U8*)&(inventory.extended_number_of_points_by_return[i+1])))
        {
          Rcpp::Rcerr << "ERROR: updating header->extended_number_of_points_by_return[" << i << "]" << std::endl;
          return FALSE;
        }
      }
    }
  }
  else
  {
    U32 number;
    stream->seek(header_start_position+107);
    if (header->point_data_format >= 6)
    {
      number = 0; // legacy counters are zero for new point types
    }
    else
    {
      number = header->number_of_point_records;
    }
    if (!stream->put32bitsLE((U8*)&number))
    {
      Rcpp::Rcerr << "ERROR: updating header->number_of_point_records" << std::endl;
      return FALSE;
    }
    npoints = header->number_of_point_records;
    for (i = 0; i < 5; i++)
    {
      if (header->point_data_format >= 6)
      {
        number = 0; // legacy counters are zero for new point types
      }
      else
      {
        number = header->number_of_points_by_return[i];
      }
      if (!stream->put32bitsLE((U8*)&number))
      {
        Rcpp::Rcerr << "ERROR: updating header->number_of_points_by_return[" << i << "]" << std::endl;
        return FALSE;
      }
    }
    stream->seek(header_start_position+179);
    if (!stream->put64bitsLE((U8*)&(header->max_x)))
    {
      Rcpp::Rcerr << "ERROR: updating header->max_x" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->min_x)))
    {
      Rcpp::Rcerr << "ERROR: updating header->min_x" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->max_y)))
    {
      Rcpp::Rcerr << "ERROR: updating header->max_y" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->min_y)))
    {
      Rcpp::Rcerr << "ERROR: updating header->min_y" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->max_z)))
    {
      Rcpp::Rcerr << "ERROR: updating header->max_z" << std::endl;
      return FALSE;
    }
    if (!stream->put64bitsLE((U8*)&(header->min_z)))
    {
      Rcpp::Rcerr << "ERROR: updating header->min_z" << std::endl;
      return FALSE;
    }
    // special handling for LAS 1.3 or higher.
    if (header->version_minor >= 3)
    {
      // nobody currently includes waveform. we set the field always to zero
      if (header->start_of_waveform_data_packet_record != 0)
      {
#ifdef _WIN32
        Rcpp::Rcerr << "WARNING: header->start_of_waveform_data_packet_record is " << header->start_of_waveform_data_packet_record << ". writing 0 instead." << std::endl;
#else
        Rcpp::Rcerr << "WARNING: header->start_of_waveform_data_packet_record is " << header->start_of_waveform_data_packet_record << ". writing 0 instead." << std::endl;
#endif
        U64 start_of_waveform_data_packet_record = 0;
        if (!stream->put64bitsLE((U8*)&start_of_waveform_data_packet_record))
        {
          Rcpp::Rcerr << "ERROR: updating start_of_waveform_data_packet_record" << std::endl;
          return FALSE;
        }
      }
      else
      {
        if (!stream->put64bitsLE((U8*)&(header->start_of_waveform_data_packet_record)))
        {
          Rcpp::Rcerr << "ERROR: updating header->start_of_waveform_data_packet_record" << std::endl;
          return FALSE;
        }
      }
    }
    // special handling for LAS 1.4 or higher.
    if (header->version_minor >= 4)
    {
      stream->seek(header_start_position+235);
      if (!stream->put64bitsLE((U8*)&(header->start_of_first_extended_variable_length_record)))
      {
        Rcpp::Rcerr << "ERROR: updating header->start_of_first_extended_variable_length_record" << std::endl;
        return FALSE;
      }
      if (!stream->put32bitsLE((U8*)&(header->number_of_extended_variable_length_records)))
      {
        Rcpp::Rcerr << "ERROR: updating header->number_of_extended_variable_length_records" << std::endl;
        return FALSE;
      }
      U64 value;
      if (header->number_of_point_records)
        value = header->number_of_point_records;
      else
        value = header->extended_number_of_point_records;
      if (!stream->put64bitsLE((U8*)&value))
      {
        Rcpp::Rcerr << "ERROR: updating header->extended_number_of_point_records" << std::endl;
        return FALSE;
      }
      for (i = 0; i < 15; i++)
      {
        if ((i < 5) && header->number_of_points_by_return[i])
          value = header->number_of_points_by_return[i];
        else
          value = header->extended_number_of_points_by_return[i];
        if (!stream->put64bitsLE((U8*)&value))
        {
          Rcpp::Rcerr << "ERROR: updating header->extended_number_of_points_by_return[" << i << "]" << std::endl;
          return FALSE;
        }
      }
    }
  }
  stream->seekEnd();
  if (update_extra_bytes)
  {
    if (header == 0)
    {
      Rcpp::Rcerr << "ERROR: header pointer is zero" << std::endl;
      return FALSE;
    }
    if (header->number_attributes)
    {
      I64 start = header_start_position + header->header_size;
      for (i = 0; i < (I32)header->number_of_variable_length_records; i++)
      {
        start += 54;
        if ((header->vlrs[i].record_id == 4) && (strcmp(header->vlrs[i].user_id, "LASF_Spec") == 0))
        {
          break;
        }
        else
        {
          start += header->vlrs[i].record_length_after_header;
        }
      }
      if (i == (I32)header->number_of_variable_length_records)
      {
        Rcpp::Rcerr << "WARNING: could not find extra bytes VLR for update" << std::endl;
      }
      else
      {
        stream->seek(start);
        if (!stream->putBytes((U8*)header->vlrs[i].data, header->vlrs[i].record_length_after_header))
        {
          Rcpp::Rcerr << "ERROR: writing " << header->vlrs[i].record_length_after_header << " bytes of data from header->vlrs[" << i << "].data" << std::endl;
          return FALSE;
        }
      }
    }
    stream->seekEnd();
  }
  return TRUE;
}

I64 LASwriterLAS::close(BOOL update_header)
{
  I64 bytes = 0;

  if (p_count != npoints)
  {
#ifdef _WIN32
    Rcpp::Rcerr << "WARNING: written " << p_count << " points but expected " << npoints << " points" << std::endl;
#else
    Rcpp::Rcerr << "WARNING: written " << p_count << " points but expected " << npoints << " points" << std::endl;
#endif
  }

  if (writer)
  {
    writer->done();
    delete writer;
    writer = 0;
  }

  if (stream)
  {
    if (update_header && p_count != npoints)
    {
      if (!stream->isSeekable())
      {
#ifdef _WIN32
        Rcpp::Rcerr << "WARNING: stream not seekable. cannot update header from " << npoints << " to " << p_count << " points." << std::endl;
#else
        Rcpp::Rcerr << "WARNING: stream not seekable. cannot update header from " << npoints << " to " << p_count << " points." << std::endl;
#endif
      }
      else
      {
        U32 number;
        if (writing_new_point_type)
        {
          number = 0;
        }
        else if (p_count > U32_MAX)
        {
          if (writing_las_1_4)
          {
            number = 0;
          }
          else
          {
            number = U32_MAX;
          }
        }
        else
        {
          number = (U32)p_count;
        }
	      stream->seek(header_start_position+107);
	      stream->put32bitsLE((U8*)&number);
        if (writing_las_1_4)
        {
  	      stream->seek(header_start_position+235+12);
  	      stream->put64bitsLE((U8*)&p_count);
        }
        stream->seekEnd();
      }
    }
    bytes = stream->tell() - header_start_position;
    delete stream;
    stream = 0;
  }

  if (file)
  {
    fclose(file);
    file = 0;
  }

  npoints = p_count;
  p_count = 0;

  return bytes;
}

LASwriterLAS::LASwriterLAS()
{
  file = 0;
  stream = 0;
  writer = 0;
  writing_las_1_4 = FALSE;
  writing_new_point_type = FALSE;
}

LASwriterLAS::~LASwriterLAS()
{
  if (writer || stream) close();
}
