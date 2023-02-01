/* This tool copies a Minix Disk image to the 'B' Flash SSD, it assumes
 * that the disk is a 128K Flash and the source file for the image is
 * LOC::M:\MINIX.DSK
 * It is rather slow at present but uses the SSD driver stuff so should 
 * pick up any improvements as that developes.
 */

#include "writedisk.h"

/* NOTE: Each time this is run it erases the SSD */

int main(void)
{
    unsigned int high_address, low_address;
    unsigned char data, not_done;

    /* need to erase each chip in the SSD */
    LCD_Position(0, 0);
    LCD_WriteChar('E');
    LCD_WriteChar('r');
    LCD_WriteChar('a');
    LCD_WriteChar('s');
    LCD_WriteChar('e');
    ssd_erase4(0);

    open_file(filename);

    LCD_Position(0, 1);
    LCD_WriteChar('P');
    LCD_WriteChar('r');
    LCD_WriteChar('o');
    LCD_WriteChar('g');
    LCD_WriteChar('r');
    LCD_WriteChar('a');
    LCD_WriteChar('m');
    for (high_address = 0; high_address < 2; high_address++) {
	low_address = 0;
	not_done = 1;

	while (not_done) {
	    data = read_byte();
	    LCD_Position(0, 2);
	    LCD_WriteChar(data);
	    ssd_write4(high_address, low_address, data);
	    low_address++;
	    if (low_address == 0)
		not_done = 0;
	}
    }

    close_file();

    LCD_Position(0, 3);
    LCD_WriteChar('D');
    LCD_WriteChar('o');
    LCD_WriteChar('n');
    LCD_WriteChar('e');
    LCD_WriteChar('.');

    while (1)
	/* Do nothing */;

    return 0;
}
