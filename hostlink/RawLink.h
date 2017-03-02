#ifndef _RAWLINK_H_
#define _RAWLINK_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "JtagAtlantic.h"

class RawLink {
  JTAGATLANTIC* jtag;

  void open() {
    if (jtag == NULL) {
      jtag = jtagatlantic_open(getenv("CABLE"), 0, 0, "hostlink");
      if (jtag == NULL) {
        fprintf(stderr, "Error opening JTAG UART\n");
        exit(EXIT_FAILURE);
      }
    }
  }

 public:
  RawLink() {
    jtag = NULL;
  }

  void get(void* buffer, int count) {
    // Open JTAG UART if not already opened
    if (jtag == NULL) open();
    // Read bytes
    uint8_t* ptr = (uint8_t*) buffer;
    while (count > 0) {
      int n = jtagatlantic_read(jtag, (char*) ptr, count);
      if (n < 0) {
        fprintf(stderr, "Error reading from JTAG UART\n");
        exit(EXIT_FAILURE);
      }
      ptr += n;
      count -= n;
    }
  }

  bool canGet() {
    return jtagatlantic_bytes_available(jtag) > 0;
  }

  void put(void* buffer, int count) {
    // Open JTAG UART if not already opened
    if (jtag == NULL) open();
    // Write bytes
    uint8_t* ptr = (uint8_t*) buffer;
    while (count > 0) {
      int n = jtagatlantic_write(jtag, (char*) ptr, count);
      if (n < 0) {
        fprintf(stderr, "Error writing to JTAG UART\n");
        exit(EXIT_FAILURE);
      }
      ptr += n;
      count -= n;
    }
  }

  void flush() {
    if (jtag != NULL) jtagatlantic_flush(jtag);
  }

  ~RawLink() {
    if (jtag != NULL) jtagatlantic_close(jtag);
  }
};

#endif
