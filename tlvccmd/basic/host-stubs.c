/*
 * Architecture Specific stubs
 */
#include <stdio.h>
#include <stdlib.h>
#if __ia16__
#include <arch/io.h>
#else
#define inb(port)           0
#define inw(port)           0
#define outb(value,port)
#define outw(value,port)
#endif
#include "host.h"
#include "basic.h"

extern FILE *outfile;

void host_digitalWrite(int pin,int state) {
}

int host_digitalRead(int pin) {
    return 0;
}

int host_analogRead(int pin) {
	return 0;
}

void host_pinMode(int pin,int mode) {
}

void host_mode(int mode) {
}

void host_cls() {
	fprintf(outfile, "\033[H\033[2J");
}

void host_color(int fgc, int bgc) {
}

void host_plot(int x, int y) {
}

void host_draw(int x, int y) {
}

void host_circle(int x, int y, int r) {
}

void host_outb(int port, int value) {
    outb(value, port);
}

void host_outw(int port, int value) {
    outw(value, port);
}

int host_inpb(int port) {
    return inb(port);
}

int host_inpw(int port) {
    return inw(port);
}
