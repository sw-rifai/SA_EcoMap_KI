# mask cloud in hyperspectral data

import rasterio
import numpy as np

# --- set file paths ---
hyperspectral_path = 'C:/Users/Annie/Documents/local_docs/ki-ecology/SmartSat/corrected/HS-L1CR-FF-0616-20250321_L2A.tif'
output_mask_path = 'C:/Users/Annie/Documents/local_docs/ki-ecology/SmartSat/model_outputs/HS-L1CR-FF-0616-20250321_L2A_cloud_mask.tif'


# --- Custom Cloud Definition ---
# A pixel is a cloud if:
# 1. At least this many bands have a value >= VERY_BRIGHT_THRESHOLD
COUNT_OF_VERY_BRIGHT_BANDS = 35 
# 2. AND the dimmest band in that pixel is still brighter than this threshold
MINIMUM_PIXEL_VALUE_THRESHOLD = 2000
# The threshold to define a "very bright" band
VERY_BRIGHT_THRESHOLD = 3000

print("Applying advanced cloud masking logic")

with rasterio.open(hyperspectral_path) as src:
    # Read the data as a masked array to automatically handle nodata values
    masked_data = src.read(masked=True)

    # --- Condition 1: Check if enough bands are VERY bright ---
    
    # Create a boolean array: True where a band value is >= 3000
    is_very_bright = masked_data >= VERY_BRIGHT_THRESHOLD
    
    # Count how many bands are "very bright" for each pixel by summing along the bands axis
    very_bright_count = np.sum(is_very_bright, axis=0)
    
    # Create the first mask: True where the count of very bright bands is sufficient
    condition1_met = very_bright_count >= COUNT_OF_VERY_BRIGHT_BANDS

    # --- Condition 2: Check that NO band is too dim ---

    # Find the minimum band value for each pixel.
    min_value_per_pixel = np.min(masked_data, axis=0)
    
    # Create the second mask: True where the dimmest band is still above minimum threshold
    condition2_met = min_value_per_pixel > MINIMUM_PIXEL_VALUE_THRESHOLD

    # --- Combine Conditions ---

    # The final cloud mask is where BOTH conditions are met for a pixel
    # The '&' operator performs a logical AND between the two boolean arrays
    cloud_mask = condition1_met & condition2_met

    # Convert the final boolean mask (which might have masked areas) to a regular
    # numpy array, filling any original nodata areas with False (not a cloud)
    final_mask = cloud_mask.filled(False)

    # --- Save the Output ---

    # Get the metadata from the original file to create a geo-referenced output
    meta = src.meta.copy()
    meta.update({
        'dtype': 'uint8',  # Use 8-bit integer for a simple 0/1 mask
        'count': 1,        # The mask is a single band
        'nodata': 255      # value to represent nodata in the output mask
    })

    # Write the final mask to a new GeoTIFF file.
    with rasterio.open(output_mask_path, 'w', **meta) as dst:
        dst.write(final_mask.astype(np.uint8), 1)

print(f"Cloud mask saved to: {output_mask_path}")
