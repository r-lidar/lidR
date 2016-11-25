/*
===============================================================================

  FILE:  lasreader_las.cpp
  
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
#include "lasreader_las.hpp"

#include "bytestreamin.hpp"
#include "bytestreamin_file.hpp"
#include "bytestreamin_istream.hpp"
#include "lasreadpoint.hpp"
#include "lasindex.hpp"

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif


#include <Rcpp.h>

BOOL LASreaderLAS::open(const char* file_name, I32 io_buffer_size, BOOL peek_only)
{
  if (file_name == 0)
  {
    Rcpp::Rcerr << "ERROR: fine name pointer is zero" << std::endl;
    return FALSE;
  }

  file = fopen(file_name, "rb");
  if (file == 0)
  {
    Rcpp::Rcerr << "ERROR: cannot open file '" << file_name << "'" << std::endl;
    return FALSE;
  }

  if (setvbuf(file, NULL, _IOFBF, io_buffer_size) != 0)
  {
    Rcpp::Rcerr << "WARNING: setvbuf() failed with buffer size " << io_buffer_size << "" << std::endl;
  }

  // create input
  ByteStreamIn* in;
  if (IS_LITTLE_ENDIAN())
    in = new ByteStreamInFileLE(file);
  else
    in = new ByteStreamInFileBE(file);

  return open(in, peek_only);
}

BOOL LASreaderLAS::open(FILE* file, BOOL peek_only)
{
  if (file == 0)
  {
    Rcpp::Rcerr << "ERROR: file pointer is zero" << std::endl;
    return FALSE;
  }

#ifdef _WIN32
  if (file == stdin)
  {
    if(_setmode( _fileno( stdin ), _O_BINARY ) == -1 )
    {
      Rcpp::Rcerr << "ERROR: cannot set stdin to binary (untranslated) mode" << std::endl;
      return FALSE;
    }
  }
#endif

  // create input
  ByteStreamIn* in;
  if (IS_LITTLE_ENDIAN())
    in = new ByteStreamInFileLE(file);
  else
    in = new ByteStreamInFileBE(file);

  return open(in);
}

BOOL LASreaderLAS::open(istream& stream, BOOL peek_only)
{
  // create input
  ByteStreamIn* in;
  if (IS_LITTLE_ENDIAN())
    in = new ByteStreamInIstreamLE(stream);
  else
    in = new ByteStreamInIstreamBE(stream);

  return open(in, peek_only);
}

BOOL LASreaderLAS::open(ByteStreamIn* stream, BOOL peek_only)
{
  U32 i,j;

  if (stream == 0)
  {
    Rcpp::Rcerr << "ERROR: ByteStreamIn* pointer is zero" << std::endl;
    return FALSE;
  }

  this->stream = stream;

  // clean the header

  header.clean();

  // read the header variable after variable (to avoid alignment issues)

  try { stream->getBytes((U8*)header.file_signature, 4); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.file_signature" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.file_source_ID)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.file_source_ID" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.global_encoding)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.global_encoding" << std::endl;
    return FALSE;
  }
  try { stream->get32bitsLE((U8*)&(header.project_ID_GUID_data_1)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.project_ID_GUID_data_1" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.project_ID_GUID_data_2)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.project_ID_GUID_data_2" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.project_ID_GUID_data_3)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.project_ID_GUID_data_3" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)header.project_ID_GUID_data_4, 8); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.project_ID_GUID_data_4" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)&(header.version_major), 1); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.version_major" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)&(header.version_minor), 1); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.version_minor" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)header.system_identifier, 32); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.system_identifier" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)header.generating_software, 32); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.generating_software" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.file_creation_day)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.file_creation_day" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.file_creation_year)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.file_creation_year" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.header_size)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.header_size" << std::endl;
    return FALSE;
  }
  try { stream->get32bitsLE((U8*)&(header.offset_to_point_data)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.offset_to_point_data" << std::endl;
    return FALSE;
  }
  try { stream->get32bitsLE((U8*)&(header.number_of_variable_length_records)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.number_of_variable_length_records" << std::endl;
    return FALSE;
  }
  try { stream->getBytes((U8*)&(header.point_data_format), 1); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.point_data_format" << std::endl;
    return FALSE;
  }
  try { stream->get16bitsLE((U8*)&(header.point_data_record_length)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.point_data_record_length" << std::endl;
    return FALSE;
  }
  try { stream->get32bitsLE((U8*)&(header.number_of_point_records)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.number_of_point_records" << std::endl;
    return FALSE;
  }
  for (i = 0; i < 5; i++)
  {
    try { stream->get32bitsLE((U8*)&(header.number_of_points_by_return[i])); } catch(...)
    {
      Rcpp::Rcerr << "ERROR: reading header.number_of_points_by_return " << i << "" << std::endl;
      return FALSE;
    }
  }
  try { stream->get64bitsLE((U8*)&(header.x_scale_factor)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.x_scale_factor" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.y_scale_factor)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.y_scale_factor" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.z_scale_factor)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.z_scale_factor" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.x_offset)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.x_offset" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.y_offset)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.y_offset" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.z_offset)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.z_offset" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.max_x)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.max_x" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.min_x)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.min_x" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.max_y)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.max_y" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.min_y)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.min_y" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.max_z)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.max_z" << std::endl;
    return FALSE;
  }
  try { stream->get64bitsLE((U8*)&(header.min_z)); } catch(...)
  {
    Rcpp::Rcerr << "ERROR: reading header.min_z" << std::endl;
    return FALSE;
  }

  // check core header contents
  if (!header.check())
  {
    return FALSE;
  }

  // special handling for LAS 1.3
  if ((header.version_major == 1) && (header.version_minor >= 3))
  {
    if (header.header_size < 235)
    {
      Rcpp::Rcerr << "WARNING: for LAS 1." << header.version_minor << " header_size should at least be 235 but it is only " << header.header_size << "" << std::endl;
      header.user_data_in_header_size = header.header_size - 227;
    }
    else
    {
      try { stream->get64bitsLE((U8*)&(header.start_of_waveform_data_packet_record)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.start_of_waveform_data_packet_record" << std::endl;
        return FALSE;
      }
      header.user_data_in_header_size = header.header_size - 235;
    }
  }
  else
  {
    header.user_data_in_header_size = header.header_size - 227;
  }

  // special handling for LAS 1.4
  if ((header.version_major == 1) && (header.version_minor >= 4))
  {
    if (header.header_size < 375)
    {
      Rcpp::Rcerr << "ERROR: for LAS 1." << header.version_minor << " header_size should at least be 375 but it is only " << header.header_size << "" << std::endl;
      return FALSE;
    }
    else
    {
      try { stream->get64bitsLE((U8*)&(header.start_of_first_extended_variable_length_record)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.start_of_first_extended_variable_length_record" << std::endl;
        return FALSE;
      }
      try { stream->get32bitsLE((U8*)&(header.number_of_extended_variable_length_records)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.number_of_extended_variable_length_records" << std::endl;
        return FALSE;
      }
      try { stream->get64bitsLE((U8*)&(header.extended_number_of_point_records)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.extended_number_of_point_records" << std::endl;
        return FALSE;
      }
      for (i = 0; i < 15; i++)
      {
        try { stream->get64bitsLE((U8*)&(header.extended_number_of_points_by_return[i])); } catch(...)
        {
          Rcpp::Rcerr << "ERROR: reading header.extended_number_of_points_by_return[" << i << "]" << std::endl;
          return FALSE;
        }
      }
      header.user_data_in_header_size = header.header_size - 375;
    }
  }

  // load any number of user-defined bytes that might have been added to the header
  if (header.user_data_in_header_size)
  {
    header.user_data_in_header = new U8[header.user_data_in_header_size];

    try { stream->getBytes((U8*)header.user_data_in_header, header.user_data_in_header_size); } catch(...)
    {
      Rcpp::Rcerr << "ERROR: reading " << header.user_data_in_header_size << " bytes of data into header.user_data_in_header" << std::endl;
      return FALSE;
    }
  }

  npoints = (header.number_of_point_records ? header.number_of_point_records : header.extended_number_of_point_records);
  p_count = 0;

  if (peek_only)
  {
    // at least repair point type in incomplete header (no VLRs, no LASzip, no LAStiling) 
    header.point_data_format &= 127;
    return TRUE;
  }

  // read the variable length records into the header

  U32 vlrs_size = 0;

  if (header.number_of_variable_length_records)
  {
    header.vlrs = (LASvlr*)malloc(sizeof(LASvlr)*header.number_of_variable_length_records);
  
    for (i = 0; i < header.number_of_variable_length_records; i++)
    {
      // make sure there are enough bytes left to read a variable length record before the point block starts

      if (((int)header.offset_to_point_data - vlrs_size - header.header_size) < 54)
      {
        Rcpp::Rcerr << "WARNING: only " << (int)header.offset_to_point_data - vlrs_size - header.header_size << " bytes until point block after reading " << i << " of " << header.number_of_variable_length_records << " vlrs. skipping remaining vlrs ..." << std::endl;
        header.number_of_variable_length_records = i;
        break;
      }

      // read variable length records variable after variable (to avoid alignment issues)

      try { stream->get16bitsLE((U8*)&(header.vlrs[i].reserved)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.vlrs[" << i << "].reserved" << std::endl;
        return FALSE;
      }

      try { stream->getBytes((U8*)header.vlrs[i].user_id, 16); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.vlrs[" << i << "].user_id" << std::endl;
        return FALSE;
      }
      try { stream->get16bitsLE((U8*)&(header.vlrs[i].record_id)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.vlrs[" << i << "].record_id" << std::endl;
        return FALSE;
      }
      try { stream->get16bitsLE((U8*)&(header.vlrs[i].record_length_after_header)); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.vlrs[" << i << "].record_length_after_header" << std::endl;
        return FALSE;
      }
      try { stream->getBytes((U8*)header.vlrs[i].description, 32); } catch(...)
      {
        Rcpp::Rcerr << "ERROR: reading header.vlrs[" << i << "].description" << std::endl;
        return FALSE;
      }

      // keep track on the number of bytes we have read so far

      vlrs_size += 54;

      // check variable length record contents

/*
      if (header.vlrs[i].reserved != 0xAABB)
      {
        Rcpp::Rcerr << "WARNING: wrong header.vlrs[" << i << "].reserved: " << header.vlrs[i].reserved << " != 0xAABB" << std::endl;
      }
*/

      // make sure there are enough bytes left to read the data of the variable length record before the point block starts

      if (((int)header.offset_to_point_data - vlrs_size - header.header_size) < header.vlrs[i].record_length_after_header)
      {
        Rcpp::Rcerr << "WARNING: only " << (int)header.offset_to_point_data - vlrs_size - header.header_size << " bytes until point block when trying to read " << header.vlrs[i].record_length_after_header << " bytes into header.vlrs[" << i << "].data" << std::endl;
        header.vlrs[i].record_length_after_header = (int)header.offset_to_point_data - vlrs_size - header.header_size;
      }

      // load data following the header of the variable length record

      if (header.vlrs[i].record_length_after_header)
      {
        if (strcmp(header.vlrs[i].user_id, "laszip encoded") == 0)
        {
          header.laszip = new LASzip();

          // read this data following the header of the variable length record
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

          try { stream->get16bitsLE((U8*)&(header.laszip->compressor)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading compressor " << (I32)header.laszip->compressor << "" << std::endl;
            return FALSE;
          }
          try { stream->get16bitsLE((U8*)&(header.laszip->coder)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading coder " << (I32)header.laszip->coder << "" << std::endl;
            return FALSE;
          }
          try { stream->getBytes((U8*)&(header.laszip->version_major), 1); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading version_major " << header.laszip->version_major << "" << std::endl;
            return FALSE;
          }
          try { stream->getBytes((U8*)&(header.laszip->version_minor), 1); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading version_minor " << header.laszip->version_minor << "" << std::endl;
            return FALSE;
          }
          try { stream->get16bitsLE((U8*)&(header.laszip->version_revision)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading version_revision " << header.laszip->version_revision << "" << std::endl;
            return FALSE;
          }
          try { stream->get32bitsLE((U8*)&(header.laszip->options)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading options " << (I32)header.laszip->options << "" << std::endl;
            return FALSE;
          }
          try { stream->get32bitsLE((U8*)&(header.laszip->chunk_size)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading chunk_size " << header.laszip->chunk_size << "" << std::endl;
            return FALSE;
          }
          try { stream->get64bitsLE((U8*)&(header.laszip->number_of_special_evlrs)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading number_of_special_evlrs " << (I32)header.laszip->number_of_special_evlrs << "" << std::endl;
            return FALSE;
          }
          try { stream->get64bitsLE((U8*)&(header.laszip->offset_to_special_evlrs)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading offset_to_special_evlrs " << (I32)header.laszip->offset_to_special_evlrs << "" << std::endl;
            return FALSE;
          }
          try { stream->get16bitsLE((U8*)&(header.laszip->num_items)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading num_items " << header.laszip->num_items << "" << std::endl;
            return FALSE;
          }
          header.laszip->items = new LASitem[header.laszip->num_items];
          for (j = 0; j < header.laszip->num_items; j++)
          {
            U16 type, size, version;
            try { stream->get16bitsLE((U8*)&type); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading type " << type << " of item " << j << "" << std::endl;
              return FALSE;
            }
            try { stream->get16bitsLE((U8*)&size); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading size " << size << " of item " << j << "" << std::endl;
              return FALSE;
            }
            try { stream->get16bitsLE((U8*)&version); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading version " << version << " of item " << j << "" << std::endl;
              return FALSE;
            }
            header.laszip->items[j].type = (LASitem::Type)type;
            header.laszip->items[j].size = size;
            header.laszip->items[j].version = version;
          }
        }
        else if (((strcmp(header.vlrs[i].user_id, "LAStools") == 0) && (header.vlrs[i].record_id == 10)) || (strcmp(header.vlrs[i].user_id, "lastools tile") == 0))
        {
          header.clean_lastiling();
          header.vlr_lastiling = new LASvlr_lastiling();

          // read the payload of this VLR which contains 28 bytes
          //   U32  level                                          4 bytes 
          //   U32  level_index                                    4 bytes 
          //   U32  implicit_levels + buffer bit + reversible bit  4 bytes 
          //   F32  min_x                                          4 bytes 
          //   F32  max_x                                          4 bytes 
          //   F32  min_y                                          4 bytes 
          //   F32  max_y                                          4 bytes 

          if (header.vlrs[i].record_length_after_header == 28)
          {
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->level)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->level " << header.vlr_lastiling->level << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->level_index)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->level_index " << header.vlr_lastiling->level_index << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE(((U8*)header.vlr_lastiling)+8); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->implicit_levels " << header.vlr_lastiling->implicit_levels << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->min_x)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->min_x " << header.vlr_lastiling->min_x << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->max_x)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->max_x " << header.vlr_lastiling->max_x << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->min_y)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->min_y " << header.vlr_lastiling->min_y << "" << std::endl;
              return FALSE;
            }
            try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->max_y)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lastiling->max_y " << header.vlr_lastiling->max_y << "" << std::endl;
              return FALSE;
            }
          }
          else
          {
            Rcpp::Rcerr << "ERROR: record_length_after_header of VLR " << header.vlrs[i].user_id << " (" << header.vlrs[i].record_id << ") is " << header.vlrs[i].record_length_after_header << " instead of 28" << std::endl;
            return FALSE;
          }
        }
        else if (((strcmp(header.vlrs[i].user_id, "LAStools") == 0) && (header.vlrs[i].record_id == 20)))
        {
          header.clean_lasoriginal();
          header.vlr_lasoriginal = new LASvlr_lasoriginal();

          // read the payload of this VLR which contains 176 bytes

          if (header.vlrs[i].record_length_after_header == 176)
          {
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->number_of_point_records)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->number_of_point_records " << (U32)header.vlr_lasoriginal->number_of_point_records << "" << std::endl;
              return FALSE;
            }
            for (j = 0; j < 15; j++)
            {
              try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->number_of_points_by_return[j])); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->number_of_points_by_return[" << j << "] " << (U32)header.vlr_lasoriginal->number_of_points_by_return[j] << "" << std::endl;
                return FALSE;
              }
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->min_x)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->min_x " << header.vlr_lasoriginal->min_x << "" << std::endl;
              return FALSE;
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->max_x)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->max_x " << header.vlr_lasoriginal->max_x << "" << std::endl;
              return FALSE;
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->min_y)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->min_y " << header.vlr_lasoriginal->min_y << "" << std::endl;
              return FALSE;
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->max_y)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->max_y " << header.vlr_lasoriginal->max_y << "" << std::endl;
              return FALSE;
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->min_z)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->min_z " << header.vlr_lasoriginal->min_z << "" << std::endl;
              return FALSE;
            }
            try { stream->get64bitsLE((U8*)&(header.vlr_lasoriginal->max_z)); } catch(...)
            {
              Rcpp::Rcerr << "ERROR: reading vlr_lasoriginal->max_z " << header.vlr_lasoriginal->max_z << "" << std::endl;
              return FALSE;
            }
          }
          else
          {
            Rcpp::Rcerr << "ERROR: record_length_after_header of VLR " << header.vlrs[i].user_id << " (" << header.vlrs[i].record_id << ") is " << header.vlrs[i].record_length_after_header << " instead of 176" << std::endl;
            return FALSE;
          }
        }
        else
        {
          header.vlrs[i].data = new U8[header.vlrs[i].record_length_after_header];

          try { stream->getBytes(header.vlrs[i].data, header.vlrs[i].record_length_after_header); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading " << header.vlrs[i].record_length_after_header << " bytes of data into header.vlrs[" << i << "].data" << std::endl;
            return FALSE;
          }
        }
      }
      else
      {
        header.vlrs[i].data = 0;
      }

      // keep track on the number of bytes we have read so far

      vlrs_size += header.vlrs[i].record_length_after_header;

      // special handling for known variable header tags

      if (strcmp(header.vlrs[i].user_id, "LASF_Projection") == 0)
      {
        if (header.vlrs[i].data)
        {
          if (header.vlrs[i].record_id == 34735) // GeoKeyDirectoryTag
          {
            if (header.vlr_geo_keys)
            {
              Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoKeyDirectoryTag" << std::endl;
            }
            header.vlr_geo_keys = (LASvlr_geo_keys*)header.vlrs[i].data;

            // check variable header geo keys contents

            if (header.vlr_geo_keys->key_directory_version != 1)
            {
              Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->key_directory_version: " << header.vlr_geo_keys->key_directory_version << " != 1" << std::endl;
            }
            if (header.vlr_geo_keys->key_revision != 1)
            {
              Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->key_revision: " << header.vlr_geo_keys->key_revision << " != 1" << std::endl;
            }
            if (header.vlr_geo_keys->minor_revision != 0)
            {
              Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->minor_revision: " << header.vlr_geo_keys->minor_revision << " != 0" << std::endl;
            }
            header.vlr_geo_key_entries = (LASvlr_key_entry*)&header.vlr_geo_keys[1];
          }
          else if (header.vlrs[i].record_id == 34736) // GeoDoubleParamsTag
          {
            if (header.vlr_geo_double_params)
            {
              Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoF64ParamsTag" << std::endl;
            }
            header.vlr_geo_double_params = (F64*)header.vlrs[i].data;
          }
          else if (header.vlrs[i].record_id == 34737) // GeoAsciiParamsTag
          {
            if (header.vlr_geo_ascii_params)
            {
              Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoAsciiParamsTag" << std::endl;
            }
            header.vlr_geo_ascii_params = (CHAR*)header.vlrs[i].data;
          }
          else if ((header.vlrs[i].record_id != 2111) && (header.vlrs[i].record_id != 2112)) // WKT OGC MATH TRANSFORM or WKT OGC COORDINATE SYSTEM
          {
            Rcpp::Rcerr << "WARNING: unknown LASF_Projection VLR with record_id " << header.vlrs[i].record_id << "." << std::endl;
          } 
        }
        else if (header.vlrs[i].record_id != 2112) // GeoAsciiParamsTag
        {
          Rcpp::Rcerr << "WARNING: no payload for LASF_Projection VLR with record_id " << header.vlrs[i].record_id << "." << std::endl;
        }
      }
      else if (strcmp(header.vlrs[i].user_id, "LASF_Spec") == 0)
      {
        if (header.vlrs[i].data)
        {
          if (header.vlrs[i].record_id == 0) // ClassificationLookup
          {
            if (header.vlr_classification)
            {
              Rcpp::Rcerr << "WARNING: variable length records contain more than one ClassificationLookup" << std::endl;
            }
            header.vlr_classification = (LASvlr_classification*)header.vlrs[i].data;
          }
          else if (header.vlrs[i].record_id == 2) // Histogram
          {
          }
          else if (header.vlrs[i].record_id == 3) // TextAreaDescription
          {
          }
          else if (header.vlrs[i].record_id == 4) // ExtraBytes
          {
            header.init_attributes(header.vlrs[i].record_length_after_header/sizeof(LASattribute), (LASattribute*)header.vlrs[i].data);
          }
          else if ((header.vlrs[i].record_id >= 100) && (header.vlrs[i].record_id < 355)) // WavePacketDescriptor
          {
            I32 idx = header.vlrs[i].record_id - 99;

            if (header.vlr_wave_packet_descr == 0)
            {
              header.vlr_wave_packet_descr = new LASvlr_wave_packet_descr*[256];
              for (j = 0; j < 256; j++) header.vlr_wave_packet_descr[j] = 0;
            }
            if (header.vlr_wave_packet_descr[idx])
            {
              Rcpp::Rcerr << "WARNING: variable length records defines wave packet descr " << idx << " more than once" << std::endl;
            }
            if (header.vlrs[i].record_length_after_header != 26)
            {
              Rcpp::Rcerr << "WARNING: variable length record payload for wave packet descr " << idx << " is " << (I32)header.vlrs[i].record_length_after_header << " instead of 26 bytes" << std::endl;
            }
            header.vlr_wave_packet_descr[idx] = (LASvlr_wave_packet_descr*)header.vlrs[i].data;
            if ((header.vlr_wave_packet_descr[idx]->getBitsPerSample() != 8) && (header.vlr_wave_packet_descr[idx]->getBitsPerSample() != 16))
            {
              Rcpp::Rcerr << "WARNING: bits per sample for wave packet descr " << idx << " is " << (I32)header.vlr_wave_packet_descr[idx]->getBitsPerSample() << " instead of 8 or 16" << std::endl;
            }
            if (header.vlr_wave_packet_descr[idx]->getNumberOfSamples() == 0)
            {
              Rcpp::Rcerr << "WARNING: number of samples for wave packet descr " << idx << " is zero" << std::endl;
            }
            if (header.vlr_wave_packet_descr[idx]->getNumberOfSamples() > 8096)
            {
              Rcpp::Rcerr << "WARNING: number of samples of " << header.vlr_wave_packet_descr[idx]->getNumberOfSamples() << " for wave packet descr " << idx << " is with unusually large" << std::endl;
            }
            if (header.vlr_wave_packet_descr[idx]->getTemporalSpacing() == 0)
            {
              Rcpp::Rcerr << "WARNING: temporal spacing for wave packet descr " << idx << " is zero" << std::endl;
            }
/*
            // fix for RiPROCESS export error
            if (idx == 1)
              header.vlr_wave_packet_descr[idx]->setNumberOfSamples(80);
            else
              header.vlr_wave_packet_descr[idx]->setNumberOfSamples(160);

            // fix for Optech LMS export error
            header.vlr_wave_packet_descr[idx]->setNumberOfSamples(header.vlr_wave_packet_descr[idx]->getNumberOfSamples()/2);
*/
          }
        }
        else
        {
          Rcpp::Rcerr << "WARNING: no payload for LASF_Spec (not specification-conform)." << std::endl;
        }
      }
      else if ((strcmp(header.vlrs[i].user_id, "laszip encoded") == 0) || ((strcmp(header.vlrs[i].user_id, "LAStools") == 0) && (header.vlrs[i].record_id < 2000)) || (strcmp(header.vlrs[i].user_id, "lastools tile") == 0))
      {
        // we take our own VLRs with record IDs below 2000 away from everywhere
        header.offset_to_point_data -= (54+header.vlrs[i].record_length_after_header);
        vlrs_size -= (54+header.vlrs[i].record_length_after_header);
        i--;
        header.number_of_variable_length_records--;
      }
    }
  }

  // load any number of user-defined bytes that might have been added after the header

  header.user_data_after_header_size = (int)header.offset_to_point_data - vlrs_size - header.header_size;
  if (header.user_data_after_header_size)
  {
    header.user_data_after_header = new U8[header.user_data_after_header_size];

    try { stream->getBytes((U8*)header.user_data_after_header, header.user_data_after_header_size); } catch(...)
    {
      Rcpp::Rcerr << "ERROR: reading " << header.user_data_after_header_size << " bytes of data into header.user_data_after_header" << std::endl;
      return FALSE;
    }
  }

  // special handling for LAS 1.4
  if ((header.version_major == 1) && (header.version_minor >= 4))
  {
    if (header.number_of_extended_variable_length_records)
    {
      if (!stream->isSeekable())
      {
        Rcpp::Rcerr << "WARNING: LAS " << header.version_major << "." << header.version_minor << " file has " << header.number_of_extended_variable_length_records << " EVLRs but stream is not seekable ..." << std::endl;
      }
      else
      {
        I64 here = stream->tell();
        stream->seek(header.start_of_first_extended_variable_length_record);

        header.evlrs = (LASevlr*)malloc(sizeof(LASevlr)*header.number_of_extended_variable_length_records);
  
        // read the extended variable length records into the header

        I64 evlrs_size = 0;

        for (i = 0; i < header.number_of_extended_variable_length_records; i++)
        {
          // read variable length records variable after variable (to avoid alignment issues)

          try { stream->get16bitsLE((U8*)&(header.evlrs[i].reserved)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading header.evlrs[" << i << "].reserved" << std::endl;
            return FALSE;
          }
          try { stream->getBytes((U8*)header.evlrs[i].user_id, 16); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading header.evlrs[" << i << "].user_id" << std::endl;
            return FALSE;
          }
          try { stream->get16bitsLE((U8*)&(header.evlrs[i].record_id)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading header.evlrs[" << i << "].record_id" << std::endl;
            return FALSE;
          }
          try { stream->get64bitsLE((U8*)&(header.evlrs[i].record_length_after_header)); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading header.evlrs[" << i << "].record_length_after_header" << std::endl;
            return FALSE;
          }
          try { stream->getBytes((U8*)header.evlrs[i].description, 32); } catch(...)
          {
            Rcpp::Rcerr << "ERROR: reading header.evlrs[" << i << "].description" << std::endl;
            return FALSE;
          }

          // keep track on the number of bytes we have read so far

          evlrs_size += 60;

          // check variable length record contents

/*
          if (header.evlrs[i].reserved != 0)
          {
            Rcpp::Rcerr << "WARNING: wrong header.evlrs[" << i << "].reserved: " << header.evlrs[i].reserved << " != 0" << std::endl;
          }
*/

          // load data following the header of the variable length record

          if (header.evlrs[i].record_length_after_header)
          {
            if (strcmp(header.evlrs[i].user_id, "laszip encoded") == 0)
            {
              header.laszip = new LASzip();

              // read this data following the header of the variable length record
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

              try { stream->get16bitsLE((U8*)&(header.laszip->compressor)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading compressor " << (I32)header.laszip->compressor << "" << std::endl;
                return FALSE;
              }
              try { stream->get16bitsLE((U8*)&(header.laszip->coder)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading coder " << (I32)header.laszip->coder << "" << std::endl;
                return FALSE;
              }
              try { stream->getBytes((U8*)&(header.laszip->version_major), 1); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading version_major " << header.laszip->version_major << "" << std::endl;
                return FALSE;
              }
              try { stream->getBytes((U8*)&(header.laszip->version_minor), 1); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading version_minor " << header.laszip->version_minor << "" << std::endl;
                return FALSE;
              }
              try { stream->get16bitsLE((U8*)&(header.laszip->version_revision)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading version_revision " << header.laszip->version_revision << "" << std::endl;
                return FALSE;
              }
              try { stream->get32bitsLE((U8*)&(header.laszip->options)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading options " << (I32)header.laszip->options << "" << std::endl;
                return FALSE;
              }
              try { stream->get32bitsLE((U8*)&(header.laszip->chunk_size)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading chunk_size " << header.laszip->chunk_size << "" << std::endl;
                return FALSE;
              }
              try { stream->get64bitsLE((U8*)&(header.laszip->number_of_special_evlrs)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading number_of_special_evlrs " << (I32)header.laszip->number_of_special_evlrs << "" << std::endl;
                return FALSE;
              }
              try { stream->get64bitsLE((U8*)&(header.laszip->offset_to_special_evlrs)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading offset_to_special_evlrs " << (I32)header.laszip->offset_to_special_evlrs << "" << std::endl;
                return FALSE;
              }
              try { stream->get16bitsLE((U8*)&(header.laszip->num_items)); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading num_items " << header.laszip->num_items << "" << std::endl;
                return FALSE;
              }
              header.laszip->items = new LASitem[header.laszip->num_items];
              for (j = 0; j < header.laszip->num_items; j++)
              {
                U16 type, size, version;
                try { stream->get16bitsLE((U8*)&type); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading type " << type << " of item " << j << "" << std::endl;
                  return FALSE;
                }
                try { stream->get16bitsLE((U8*)&size); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading size " << size << " of item " << j << "" << std::endl;
                  return FALSE;
                }
                try { stream->get16bitsLE((U8*)&version); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading version " << version << " of item " << j << "" << std::endl;
                  return FALSE;
                }
                header.laszip->items[j].type = (LASitem::Type)type;
                header.laszip->items[j].size = size;
                header.laszip->items[j].version = version;
              }
            }
            else if ((strcmp(header.evlrs[i].user_id, "LAStools") == 0) && (header.evlrs[i].record_id == 10))
            {
              header.clean_lastiling();
              header.vlr_lastiling = new LASvlr_lastiling();

              // read the payload of this VLR which contains 28 bytes
              //   U32  level                                          4 bytes 
              //   U32  level_index                                    4 bytes 
              //   U32  implicit_levels + buffer bit + reversible bit  4 bytes 
              //   F32  min_x                                          4 bytes 
              //   F32  max_x                                          4 bytes 
              //   F32  min_y                                          4 bytes 
              //   F32  max_y                                          4 bytes 

              if (header.evlrs[i].record_length_after_header == 28)
              {
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->level)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->level " << header.vlr_lastiling->level << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->level_index)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->level_index " << header.vlr_lastiling->level_index << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE(((U8*)header.vlr_lastiling)+8); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->implicit_levels " << header.vlr_lastiling->implicit_levels << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->min_x)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->min_x " << header.vlr_lastiling->min_x << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->max_x)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->max_x " << header.vlr_lastiling->max_x << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->min_y)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->min_y " << header.vlr_lastiling->min_y << "" << std::endl;
                  return FALSE;
                }
                try { stream->get32bitsLE((U8*)&(header.vlr_lastiling->max_y)); } catch(...)
                {
                  Rcpp::Rcerr << "ERROR: reading vlr_lastiling->max_y " << header.vlr_lastiling->max_y << "" << std::endl;
                  return FALSE;
                }
              }
              else
              {
                Rcpp::Rcerr << "ERROR: record_length_after_header of EVLR " << header.evlrs[i].user_id << " (" << header.evlrs[i].record_id << ") is " << (U32)header.evlrs[i].record_length_after_header << " instead of 28" << std::endl;
                return FALSE;
              }
            }
            else
            {
              header.evlrs[i].data = new U8[(U32)header.evlrs[i].record_length_after_header];

              try { stream->getBytes(header.evlrs[i].data, (U32)header.evlrs[i].record_length_after_header); } catch(...)
              {
                Rcpp::Rcerr << "ERROR: reading " << (I32)header.evlrs[i].record_length_after_header << " bytes of data into header.evlrs[" << i << "].data" << std::endl;
                return FALSE;
              }
            }
          }
          else
          {
            header.evlrs[i].data = 0;
          }

          // keep track on the number of bytes we have read so far

          evlrs_size += header.evlrs[i].record_length_after_header;

          // special handling for known variable header tags

          if (strcmp(header.evlrs[i].user_id, "LASF_Projection") == 0)
          {
            if (header.evlrs[i].record_id == 34735) // GeoKeyDirectoryTag
            {
              if (header.vlr_geo_keys)
              {
                Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoKeyDirectoryTag" << std::endl;
              }
              header.vlr_geo_keys = (LASvlr_geo_keys*)header.evlrs[i].data;

              // check variable header geo keys contents

              if (header.vlr_geo_keys->key_directory_version != 1)
              {
                Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->key_directory_version: " << header.vlr_geo_keys->key_directory_version << " != 1" << std::endl;
              }
              if (header.vlr_geo_keys->key_revision != 1)
              {
                Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->key_revision: " << header.vlr_geo_keys->key_revision << " != 1" << std::endl;
              }
              if (header.vlr_geo_keys->minor_revision != 0)
              {
                Rcpp::Rcerr << "WARNING: wrong vlr_geo_keys->minor_revision: " << header.vlr_geo_keys->minor_revision << " != 0" << std::endl;
              }
              header.vlr_geo_key_entries = (LASvlr_key_entry*)&header.vlr_geo_keys[1];
            }
            else if (header.evlrs[i].record_id == 34736) // GeoDoubleParamsTag
            {
              if (header.vlr_geo_double_params)
              {
                Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoF64ParamsTag" << std::endl;
              }
              header.vlr_geo_double_params = (F64*)header.evlrs[i].data;
            }
            else if (header.evlrs[i].record_id == 34737) // GeoAsciiParamsTag
            {
              if (header.vlr_geo_ascii_params)
              {
                Rcpp::Rcerr << "WARNING: variable length records contain more than one GeoAsciiParamsTag" << std::endl;
              }
              header.vlr_geo_ascii_params = (CHAR*)header.evlrs[i].data;
            }
          }
          else if (strcmp(header.evlrs[i].user_id, "LASF_Spec") == 0)
          {
            if (header.evlrs[i].record_id == 0) // ClassificationLookup
            {
              if (header.vlr_classification)
              {
                Rcpp::Rcerr << "WARNING: variable length records contain more than one ClassificationLookup" << std::endl;
              }
              header.vlr_classification = (LASvlr_classification*)header.evlrs[i].data;
            }
            else if (header.evlrs[i].record_id == 2) // Histogram
            {
            }
            else if (header.evlrs[i].record_id == 3) // TextAreaDescription
            {
            }
            else if (header.evlrs[i].record_id == 4) // ExtraBytes
            {
              header.init_attributes((U32)header.evlrs[i].record_length_after_header/sizeof(LASattribute), (LASattribute*)header.evlrs[i].data);
            }
            else if ((header.evlrs[i].record_id >= 100) && (header.evlrs[i].record_id < 355)) // WavePacketDescriptor
            {
              I32 idx = header.evlrs[i].record_id - 99;

              if (header.vlr_wave_packet_descr == 0)
              {
                header.vlr_wave_packet_descr = new LASvlr_wave_packet_descr*[256];
                for (j = 0; j < 256; j++) header.vlr_wave_packet_descr[j] = 0;
              }
              if (header.vlr_wave_packet_descr[idx])
              {
                Rcpp::Rcerr << "WARNING: extended variable length records defines wave packet descr " << idx << " more than once" << std::endl;
              }
              header.vlr_wave_packet_descr[idx] = (LASvlr_wave_packet_descr*)header.evlrs[i].data;
            }
          }
          else if (strcmp(header.evlrs[i].user_id, "laszip encoded") == 0 || strcmp(header.evlrs[i].user_id, "LAStools") == 0)
          {
            // we take our own EVLRs away from everywhere
            evlrs_size -= (60+header.evlrs[i].record_length_after_header);
            i--;
            header.number_of_extended_variable_length_records--;
          }
        }
        stream->seek(here);
      }
    }
  }

  // check the compressor state

  if (header.laszip)
  {
    if (!header.laszip->check())
    {
      Rcpp::Rcerr << "ERROR: " << header.laszip->get_error() << "" << std::endl;
      Rcpp::Rcerr << "       please upgrade to the latest release of LAStools (with LASzip)" << std::endl;
      Rcpp::Rcerr << "       or contact 'martin.isenburg@rapidlasso.com' for assistance." << std::endl;
      return FALSE;
    }
  }

  // remove extra bits in point data type

  if ((header.point_data_format & 128) || (header.point_data_format & 64)) 
  {
    if (!header.laszip)
    {
      Rcpp::Rcerr << "ERROR: this file was compressed with an experimental version of laszip" << std::endl;
      Rcpp::Rcerr << "ERROR: please contact 'martin.isenburg@rapidlasso.com' for assistance." << std::endl;
      return FALSE;
    }
    header.point_data_format &= 127;
  }

  // create the point reader

  reader = new LASreadPoint();

  // initialize point and the reader

  if (header.laszip)
  {
    if (!point.init(&header, header.laszip->num_items, header.laszip->items, &header)) return FALSE;
    if (!reader->setup(header.laszip->num_items, header.laszip->items, header.laszip)) return FALSE;
  }
  else
  {
    if (!point.init(&header, header.point_data_format, header.point_data_record_length, &header)) return FALSE;
    if (!reader->setup(point.num_items, point.items)) return FALSE;
  }

  // maybe has internal EVLRs

  if (header.laszip && (header.laszip->number_of_special_evlrs > 0) && (header.laszip->offset_to_special_evlrs >= header.offset_to_point_data) && stream->isSeekable())
  {
    I64 here = stream->tell();
    try
    {
      I64 number = header.laszip->number_of_special_evlrs;
      I64 offset = header.laszip->offset_to_special_evlrs;
      I64 count;
      for (count = 0; count < number; count++)
      {
        stream->seek(offset + 2);
        CHAR user_id[16]; 
        stream->getBytes((U8*)user_id, 16);
        U16 record_id;
        stream->get16bitsLE((U8*)&record_id);
        if ((strcmp(user_id, "LAStools") == 0) && (record_id == 30))
        {
          stream->seek(offset + 60);
          index = new LASindex();
          if (index)
          {
            if (!index->read(stream))
            {
              delete index;
              index = 0;
            }
          }
          break;
        }
        else
        {
          I64 record_length_after_header;
          stream->get64bitsLE((U8*)&record_length_after_header);
          offset += (record_length_after_header + 60);
        }
      }
    }
    catch(...)
    {
      Rcpp::Rcerr << "ERROR: trying to read " << (U32)header.laszip->number_of_special_evlrs << " internal EVLRs. ignoring ..." << std::endl;
    }
    stream->seek(here);
  }

  if (!reader->init(stream)) return FALSE;

  checked_end = FALSE;

  return TRUE;
}

I32 LASreaderLAS::get_format() const
{
  if (header.laszip)
  {
    return (header.laszip->compressor == LASZIP_COMPRESSOR_NONE ? LAS_TOOLS_FORMAT_LAS : LAS_TOOLS_FORMAT_LAZ);
  }
  return LAS_TOOLS_FORMAT_LAS;
}

BOOL LASreaderLAS::seek(const I64 p_index)
{
  if (reader)
  {
    if (p_index < npoints)
    {
      if (reader->seek((U32)p_count, (U32)p_index))
      {
        p_count = p_index;
        return TRUE;
      }
    }
  }
  return FALSE;
}

BOOL LASreaderLAS::read_point_default()
{
  if (p_count < npoints)
  {
    if (reader->read(point.point) == FALSE)
    {
      if (reader->error())
      {
        Rcpp::Rcerr << "ERROR: '" << reader->error() << "' after " << (U32)p_count << " of " << (U32)npoints << " points" << std::endl;
      }
      else
      {
        Rcpp::Rcerr << "WARNING: end-of-file after " << (U32)p_count << " of " << (U32)npoints << " points" << std::endl;
      }
      return FALSE;
    }

/*
    // fix for OPTECH LMS export error
    if (point.have_wavepacket)
    {
      // distance in meters light travels in one nanoseconds divided by two divided by 1000
      F64 round_trip_distance_in_picoseconds = 0.299792458 / 2 / 1000; 
      F64 x = -point.wavepacket.getXt();
      F64 y = -point.wavepacket.getYt();
      F64 z = -point.wavepacket.getZt();
      F64 len = sqrt(x*x+y*y+z*z);
      x = x / len * round_trip_distance_in_picoseconds;
      y = y / len * round_trip_distance_in_picoseconds;
      z = z / len * round_trip_distance_in_picoseconds;
      point.wavepacket.setXt((F32)x);
      point.wavepacket.setYt((F32)y);
      point.wavepacket.setZt((F32)z);
//      alternative to converge on optical origin 
//      point.wavepacket.setXt(-point.wavepacket.getXt()/point.wavepacket.getLocation());
//      point.wavepacket.setYt(-point.wavepacket.getYt()/point.wavepacket.getLocation());
//      point.wavepacket.setZt(-point.wavepacket.getZt()/point.wavepacket.getLocation());
    }
*/
    p_count++;
    return TRUE;
  }
  else
  {
    if (!checked_end)
    {
      if (reader->check_end() == FALSE)
      {
        Rcpp::Rcerr << "ERROR: '" << reader->error() << "' when reaching end of encoding" << std::endl;
        p_count--;
      }
      if (reader->warning())
      {
        Rcpp::Rcerr << "WARNING: '" << reader->warning() << "'" << std::endl;
      }
      checked_end = TRUE;
    }
  }
  return FALSE;
}

ByteStreamIn* LASreaderLAS::get_stream() const
{
  return stream;
}

void LASreaderLAS::close(BOOL close_stream)
{
  if (reader) 
  {
    reader->done();
    delete reader;
    reader = 0;
  }
  if (close_stream)
  {
    if (stream)
    {
      delete stream;
      stream = 0;
    }
    if (file)
    {
      fclose(file);
      file = 0;
    }
  }
}

LASreaderLAS::LASreaderLAS()
{
  file = 0;
  stream = 0;
  reader = 0;
}

LASreaderLAS::~LASreaderLAS()
{
  if (reader || stream) close(TRUE);
}

LASreaderLASrescale::LASreaderLASrescale(F64 x_scale_factor, F64 y_scale_factor, F64 z_scale_factor, BOOL check_for_overflow) : LASreaderLAS()
{
  scale_factor[0] = x_scale_factor;
  scale_factor[1] = y_scale_factor;
  scale_factor[2] = z_scale_factor;
  this->check_for_overflow = check_for_overflow;
}

BOOL LASreaderLASrescale::read_point_default()
{
  if (!LASreaderLAS::read_point_default()) return FALSE;
  if (rescale_x)
  {
    F64 coordinate = (orig_x_scale_factor*point.get_X())/header.x_scale_factor;
    point.set_X(I32_QUANTIZE(coordinate));
  }
  if (rescale_y)
  {
    F64 coordinate = (orig_y_scale_factor*point.get_Y())/header.y_scale_factor;
    point.set_Y(I32_QUANTIZE(coordinate));
  }
  if (rescale_z)
  {
    F64 coordinate = (orig_z_scale_factor*point.get_Z())/header.z_scale_factor;
    point.set_Z(I32_QUANTIZE(coordinate));
  }
  return TRUE;
}

BOOL LASreaderLASrescale::open(ByteStreamIn* stream, BOOL peek_only)
{
  LASquantizer quantizer = header;
  if (!LASreaderLAS::open(stream, peek_only)) return FALSE;
  // do we need to change anything
  rescale_x = rescale_y = rescale_z = FALSE;
  orig_x_scale_factor = header.x_scale_factor;
  orig_y_scale_factor = header.y_scale_factor;
  orig_z_scale_factor = header.z_scale_factor;
  if (scale_factor[0] && (header.x_scale_factor != scale_factor[0]))
  {
    header.x_scale_factor = scale_factor[0];
    rescale_x = TRUE;
  }
  if (scale_factor[1] && (header.y_scale_factor != scale_factor[1]))
  {
    header.y_scale_factor = scale_factor[1];
    rescale_y = TRUE;
  }
  if (scale_factor[2] && (header.z_scale_factor != scale_factor[2]))
  {
    header.z_scale_factor = scale_factor[2];
    rescale_z = TRUE;
  }

  // (maybe) make sure rescale does not cause integer overflow for bounding box

  if (check_for_overflow)
  {
    F64 temp_f;
    I64 temp_i;

    if (rescale_x)
    {
      // make sure rescale does not cause integer overflow for min_x
      temp_f = (orig_x_scale_factor*quantizer.get_X(header.min_x))/header.x_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_x_scale_factor << " to " << header.x_scale_factor << " causes LAS integer overflow for min_x" << std::endl;
      }
      // make sure rescale does not cause integer overflow for max_x
      temp_f = (orig_x_scale_factor*quantizer.get_X(header.max_x))/header.x_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_x_scale_factor << " to " << header.x_scale_factor << " causes LAS integer overflow for max_x" << std::endl;
      }
    }

    if (rescale_y)
    {
      // make sure rescale does not cause integer overflow for min_y
      temp_f = (orig_y_scale_factor*quantizer.get_Y(header.min_y))/header.y_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_y_scale_factor << " to " << header.y_scale_factor << " causes LAS integer overflow for min_y" << std::endl;
      }
      // make sure rescale does not cause integer overflow for max_y
      temp_f = (orig_y_scale_factor*quantizer.get_Y(header.max_y))/header.y_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_y_scale_factor << " to " << header.y_scale_factor << " causes LAS integer overflow for max_y" << std::endl;
      }
    }

    if (rescale_z)
    {
      // make sure rescale does not cause integer overflow for min_z
      temp_f = (orig_z_scale_factor*quantizer.get_Z(header.min_z))/header.z_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_z_scale_factor << " to " << header.z_scale_factor << " causes LAS integer overflow for min_z" << std::endl;
      }
      // make sure rescale does not cause integer overflow for max_z
      temp_f = (orig_z_scale_factor*quantizer.get_Z(header.max_z))/header.z_scale_factor;
      temp_i = I64_QUANTIZE(temp_f);
      if (I32_FITS_IN_RANGE(temp_i) == FALSE)
      {
        Rcpp::Rcerr << "WARNING: rescaling from " << orig_z_scale_factor << " to " << header.z_scale_factor << " causes LAS integer overflow for max_z" << std::endl;
      }
    }
  }

  return TRUE;
}

LASreaderLASreoffset::LASreaderLASreoffset(F64 x_offset, F64 y_offset, F64 z_offset) : LASreaderLAS()
{
  auto_reoffset = FALSE;
  this->offset[0] = x_offset;
  this->offset[1] = y_offset;
  this->offset[2] = z_offset;
}

LASreaderLASreoffset::LASreaderLASreoffset() : LASreaderLAS()
{
  auto_reoffset = TRUE;
}

BOOL LASreaderLASreoffset::read_point_default()
{
  if (!LASreaderLAS::read_point_default()) return FALSE;
  if (reoffset_x)
  {
    F64 coordinate = ((header.x_scale_factor*point.get_X())+orig_x_offset-header.x_offset)/header.x_scale_factor;
    point.set_X(I32_QUANTIZE(coordinate));
  }
  if (reoffset_y)
  {
    F64 coordinate = ((header.y_scale_factor*point.get_Y())+orig_y_offset-header.y_offset)/header.y_scale_factor;
    point.set_Y(I32_QUANTIZE(coordinate));
  }
  if (reoffset_z)
  {
    F64 coordinate = ((header.z_scale_factor*point.get_Z())+orig_z_offset-header.z_offset)/header.z_scale_factor;
    point.set_Z(I32_QUANTIZE(coordinate));
  }
  return TRUE;
}

BOOL LASreaderLASreoffset::open(ByteStreamIn* stream, BOOL peek_only)
{
  LASquantizer quantizer = header;
  if (!LASreaderLAS::open(stream, peek_only)) return FALSE;
  // maybe auto reoffset
  if (auto_reoffset)
  {
    if (F64_IS_FINITE(header.min_x) && F64_IS_FINITE(header.max_x))
      offset[0] = ((I64)((header.min_x + header.max_x)/header.x_scale_factor/20000000))*10000000*header.x_scale_factor;
    else
      offset[0] = 0;

    if (F64_IS_FINITE(header.min_y) && F64_IS_FINITE(header.max_y))
      offset[1] = ((I64)((header.min_y + header.max_y)/header.y_scale_factor/20000000))*10000000*header.y_scale_factor;
    else
      offset[1] = 0;

    if (F64_IS_FINITE(header.min_z) && F64_IS_FINITE(header.max_z))
      offset[2] = ((I64)((header.min_z + header.max_z)/header.z_scale_factor/20000000))*10000000*header.z_scale_factor;
    else
      offset[2] = 0;
  }
  // do we need to change anything
  reoffset_x = reoffset_y = reoffset_z = FALSE;
  orig_x_offset = header.x_offset;
  orig_y_offset = header.y_offset;
  orig_z_offset = header.z_offset;
  if (header.x_offset != offset[0])
  {
    header.x_offset = offset[0];
    reoffset_x = TRUE;
  }
  if (header.y_offset != offset[1])
  {
    header.y_offset = offset[1];
    reoffset_y = TRUE;
  }
  if (header.z_offset != offset[2])
  {
    header.z_offset = offset[2];
    reoffset_z = TRUE;
  }

  // make sure reoffset does not cause integer overflow for bounding box

  F64 temp_f;
  I64 temp_i;

  if (reoffset_x)
  {
    // make sure reoffset_x does not cause integer overflow for min_x
    temp_f = ((header.x_scale_factor*quantizer.get_X(header.min_x))+orig_x_offset-header.x_offset)/header.x_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_x_offset << " to " << header.x_offset << " causes LAS integer overflow for min_x" << std::endl;
    }
    // make sure reoffset_x does not cause integer overflow for max_x
    temp_f = ((header.x_scale_factor*quantizer.get_X(header.max_x))+orig_x_offset-header.x_offset)/header.x_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_x_offset << " to " << header.x_offset << " causes LAS integer overflow for max_x" << std::endl;
    }
  }

  if (reoffset_y)
  {
    // make sure reoffset_y does not cause integer overflow for min_y
    temp_f = ((header.y_scale_factor*quantizer.get_Y(header.min_y))+orig_y_offset-header.y_offset)/header.y_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_y_offset << " to " << header.y_offset << " causes LAS integer overflow for min_y" << std::endl;
    }
    // make sure reoffset_y does not cause integer overflow for max_y
    temp_f = ((header.y_scale_factor*quantizer.get_Y(header.max_y))+orig_y_offset-header.y_offset)/header.y_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_y_offset << " to " << header.y_offset << " causes LAS integer overflow for max_y" << std::endl;
    }
  }

  if (reoffset_z)
  {
     // make sure reoffset does not cause integer overflow for min_z
    temp_f = ((header.z_scale_factor*quantizer.get_Z(header.min_z))+orig_z_offset-header.z_offset)/header.z_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_z_offset << " to " << header.z_offset << " causes LAS integer overflow for min_z" << std::endl;
    }
    // make sure rescale does not cause integer overflow for max_z
    temp_f = ((header.z_scale_factor*quantizer.get_Z(header.max_z))+orig_z_offset-header.z_offset)/header.z_scale_factor;
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: reoffsetting from " << orig_z_offset << " to " << header.z_offset << " causes LAS integer overflow for max_z" << std::endl;
    }
  }

  return TRUE;
}

LASreaderLASrescalereoffset::LASreaderLASrescalereoffset(F64 x_scale_factor, F64 y_scale_factor, F64 z_scale_factor, F64 x_offset, F64 y_offset, F64 z_offset) : LASreaderLASrescale(x_scale_factor, y_scale_factor, z_scale_factor, FALSE), LASreaderLASreoffset(x_offset, y_offset, z_offset)
{
}

LASreaderLASrescalereoffset::LASreaderLASrescalereoffset(F64 x_scale_factor, F64 y_scale_factor, F64 z_scale_factor) : LASreaderLASrescale(x_scale_factor, y_scale_factor, z_scale_factor, FALSE), LASreaderLASreoffset()
{
}

BOOL LASreaderLASrescalereoffset::read_point_default()
{
  if (!LASreaderLAS::read_point_default()) return FALSE;
  if (reoffset_x)
  {
    F64 coordinate = ((orig_x_scale_factor*point.get_X())+orig_x_offset-header.x_offset)/header.x_scale_factor;
    point.set_X(I32_QUANTIZE(coordinate));
  }
  else if (rescale_x)
  {
    F64 coordinate = (orig_x_scale_factor*point.get_X())/header.x_scale_factor;
    point.set_X(I32_QUANTIZE(coordinate));
  }
  if (reoffset_y)
  {
    F64 coordinate = ((orig_y_scale_factor*point.get_Y())+orig_y_offset-header.y_offset)/header.y_scale_factor;
    point.set_Y(I32_QUANTIZE(coordinate));
  }
  else if (rescale_y)
  {
    F64 coordinate = (orig_y_scale_factor*point.get_Y())/header.y_scale_factor;
    point.set_Y(I32_QUANTIZE(coordinate));
  }
  if (reoffset_z)
  {
    F64 coordinate = ((orig_z_scale_factor*point.get_Z())+orig_z_offset-header.z_offset)/header.z_scale_factor;
    point.set_Z(I32_QUANTIZE(coordinate));
  }
  else if (rescale_z)
  {
    F64 coordinate = (orig_z_scale_factor*point.get_Z())/header.z_scale_factor;
    point.set_Z(I32_QUANTIZE(coordinate));
  }
  return TRUE;
}

BOOL LASreaderLASrescalereoffset::open(ByteStreamIn* stream, BOOL peek_only)
{
  LASquantizer quantizer = header;
  if (!LASreaderLASrescale::open(stream, peek_only)) return FALSE;
  // maybe auto reoffset
  if (auto_reoffset)
  {
    if (F64_IS_FINITE(header.min_x) && F64_IS_FINITE(header.max_x))
      offset[0] = ((I64)((header.min_x + header.max_x)/header.x_scale_factor/20000000))*10000000*header.x_scale_factor;
    else
      offset[0] = 0;

    if (F64_IS_FINITE(header.min_y) && F64_IS_FINITE(header.max_y))
      offset[1] = ((I64)((header.min_y + header.max_y)/header.y_scale_factor/20000000))*10000000*header.y_scale_factor;
    else
      offset[1] = 0;

    if (F64_IS_FINITE(header.min_z) && F64_IS_FINITE(header.max_z))
      offset[2] = ((I64)((header.min_z + header.max_z)/header.z_scale_factor/20000000))*10000000*header.z_scale_factor;
    else
      offset[2] = 0;
  }
  // do we need to change anything
  reoffset_x = reoffset_y = reoffset_z = FALSE;
  orig_x_offset = header.x_offset;
  orig_y_offset = header.y_offset;
  orig_z_offset = header.z_offset;
  if (header.x_offset != offset[0])
  {
    header.x_offset = offset[0];
    reoffset_x = TRUE;
  }
  if (header.y_offset != offset[1])
  {
    header.y_offset = offset[1];
    reoffset_y = TRUE;
  }
  if (header.z_offset != offset[2])
  {
    header.z_offset = offset[2];
    reoffset_z = TRUE;
  }

  // make sure rescale & reoffset do not cause integer overflow for bounding box

  F64 temp_f;
  I64 temp_i;

  if (reoffset_x || rescale_x)
  {
    // make sure rescale & reoffset do not cause integer overflow for min_x
    if (reoffset_x)
    {
      temp_f = ((orig_x_scale_factor*quantizer.get_X(header.min_x))+orig_x_offset-header.x_offset)/header.x_scale_factor;
    }
    else
    {
      temp_f = (orig_x_scale_factor*quantizer.get_X(header.min_x))/header.x_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_x_scale_factor << " to " << header.x_scale_factor << " and reoffsetting from " << orig_x_offset << " to " << header.x_offset << " causes LAS integer overflow for min_x" << std::endl;
    }

    // make sure rescale & reoffset do not cause integer overflow for max_x
    if (reoffset_x)
    {
      temp_f = ((orig_x_scale_factor*quantizer.get_X(header.max_x))+orig_x_offset-header.x_offset)/header.x_scale_factor;
    }
    else
    {
      temp_f = (orig_x_scale_factor*quantizer.get_X(header.max_x))/header.x_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_x_scale_factor << " to " << header.x_scale_factor << " and reoffsetting from " << orig_x_offset << " to " << header.x_offset << " causes LAS integer overflow for max_x" << std::endl;
    }
  }

  if (reoffset_y || rescale_y)
  {
    // make sure rescale & reoffset do not cause integer overflow for min_y
    if (reoffset_y)
    {
      temp_f = ((orig_y_scale_factor*quantizer.get_Y(header.min_y))+orig_y_offset-header.y_offset)/header.y_scale_factor;
    }
    else
    {
      temp_f = (orig_y_scale_factor*quantizer.get_Y(header.min_y))/header.y_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_y_scale_factor << " to " << header.y_scale_factor << " and reoffsetting from " << orig_y_offset << " to " << header.y_offset << " causes LAS integer overflow for min_y" << std::endl;
    }

    // make sure rescale & reoffset do not cause integer overflow for max_y
    if (reoffset_y)
    {
      temp_f = ((orig_y_scale_factor*quantizer.get_Y(header.max_y))+orig_y_offset-header.y_offset)/header.y_scale_factor;
    }
    else
    {
      temp_f = (orig_y_scale_factor*quantizer.get_Y(header.max_y))/header.y_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_y_scale_factor << " to " << header.y_scale_factor << " and reoffsetting from " << orig_y_offset << " to " << header.y_offset << " causes LAS integer overflow for max_y" << std::endl;
    }
  }

  if (reoffset_z || rescale_z)
  {
    // make sure rescale & reoffset do not cause integer overflow for min_z
    if (reoffset_z)
    {
      temp_f = ((orig_z_scale_factor*quantizer.get_Z(header.min_z))+orig_z_offset-header.z_offset)/header.z_scale_factor;
    }
    else
    {
      temp_f = (orig_z_scale_factor*quantizer.get_Z(header.min_z))/header.z_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_z_scale_factor << " to " << header.z_scale_factor << " and reoffsetting from " << orig_z_offset << " to " << header.z_offset << " causes LAS integer overflow for min_z" << std::endl;
    }

    // make sure rescale & reoffset do not cause integer overflow for max_z
    if (reoffset_z)
    {
      temp_f = ((orig_z_scale_factor*quantizer.get_Z(header.max_z))+orig_z_offset-header.z_offset)/header.z_scale_factor;
    }
    else
    {
      temp_f = (orig_z_scale_factor*quantizer.get_Z(header.max_z))/header.z_scale_factor;
    }
    temp_i = I64_QUANTIZE(temp_f);
    if (I32_FITS_IN_RANGE(temp_i) == FALSE)
    {
      Rcpp::Rcerr << "WARNING: rescaling from " << orig_z_scale_factor << " to " << header.z_scale_factor << " and reoffsetting from " << orig_z_offset << " to " << header.z_offset << " causes LAS integer overflow for max_z" << std::endl;
    }
  }

  return TRUE;
}
