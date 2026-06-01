library(arrow)

r2_fs <- arrow::S3FileSystem$create(
  access_key        = Sys.getenv("R2_ACCESS_KEY_ID"),
  secret_key        = Sys.getenv("R2_SECRET_ACCESS_KEY"),
  endpoint_override = paste0(Sys.getenv("R2_ACCOUNT_ID"), ".r2.cloudflarestorage.com"),
  region            = "auto"
)

bucket <- Sys.getenv("R2_BUCKET")

upload_partitioned <- function(local_dir, bucket_prefix) {
  arrow::copy_files(
    from = local_dir,
    to   = r2_fs$path(paste0(bucket, "/", bucket_prefix))
  )
  message("Uploaded: ", bucket_prefix)
}


# Partitioned — point to the folder
upload_partitioned("export/tract_boundaries/",     "tract_boundaries")
upload_partitioned("export/epa_water_parts/", "epa_water_parts")
