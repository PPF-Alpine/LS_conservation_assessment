
test_files <- aligned_files[1:5]

presence_long_test <- map_dfr(
  test_files,
  extract_species_presence,
  segment_sides = segment_sides,
  segment_sides_vect = segment_sides_vect
)



dplyr::glimpse(presence_long_test)

table(presence_long_test$presence)
table(presence_long_test$species)


plot(st_geometry(segment_buffers[segment_buffers$seg_id == target_seg, ]), border = "red")
plot(rast(test_file), add = TRUE)


r <- rast(test_files[3])
crs(r)
st_crs(segment_sides)



# fix projection !! all has to go into WGS 8857