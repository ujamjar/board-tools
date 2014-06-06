# create nios2 bsp for nios2_sdram.qsys
# uses standard bsp settings
# run from project dir

nios2-bsp hal bsp nios2_ssram.sopcinfo \
    --set hal.enable_c_plus_plus false \
    --set hal.enable_small_c_library true \
    --set hal.enable_reduced_device_drivers true 
