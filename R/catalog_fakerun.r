catalog_fakerun = function(ctg, sleep = 0.05)
{
  catalog_apply2(ctg, function(x){Sys.sleep(sleep) ; return (0)}, need_buffer = FALSE, check_alignement = FALSE, drop_null = FALSE)
  return(invisible())
}