from PIL import Image

def make_sepia_palette(color):
    palette = []
    r, g, b = color
    for i in range(255):
        palette.extend((r*i/255, g*i/255, b*i/255))
        
    return palette

def create_sepia(input_image_path,
    output_image_path):
    whitish = (255, 240, 192)
    sepia = make_sepia_palette(whitish)
    
    color_image = Image.open(input_image_path)
    
    # convert our image to gray scale
    bw = color_image.convert('L')
    
    # add the sepia toning
    bw.putpalette(sepia)
    
    # convert to RGB for easier saving
    sepia_image = bw.convert('RGB')
    
    sepia_image.save(output_image_path)

if __name__ == '__main__':
    create_sepia('querkle.png',
                 'sepia_querkle.png')
