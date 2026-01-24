module ViaConstants where

import Data.Word (Word8)

-- Interrupt Flags Register
ifr_ca2, ifr_ca1, ifr_shiftreg, ifr_cb2, ifr_cb1, ifr_timer2, ifr_timer1, ifr_irq :: Word8
ifr_ca2      = 0x01
ifr_ca1      = 0x02
ifr_shiftreg = 0x04
ifr_cb2      = 0x08
ifr_cb1      = 0x10
ifr_timer2   = 0x20
ifr_timer1   = 0x40
ifr_irq      = 0x80

-- Interrupt Enable Register
ier_ca2, ier_ca1, ier_shiftreg, ier_cb2, ier_cb1, ier_timer2, ier_timer1, ier_set_clear :: Word8
ier_ca2       = 0x01
ier_ca1       = 0x02
ier_shiftreg  = 0x04
ier_cb2       = 0x08
ier_cb1       = 0x10
ier_timer2    = 0x20
ier_timer1    = 0x40
ier_set_clear = 0x80

-- Auxiliary Control Register
acr_pa_latch_enable, acr_pb_LATCH_enable, acr_pb_shiftreg_control, acr_timer2_control, acr_timer1_continuous, acr_timer1_output_enable :: Word8
acr_pa_latch_enable      = 0x01
acr_pb_LATCH_enable      = 0x02
acr_pb_shiftreg_control  = 0x1C
acr_timer2_control       = 0x20
acr_timer1_continuous    = 0x40
acr_timer1_output_enable = 0x80

-- Peripheral Control Register
pcr_cb2_control, pcr_cb1_interrupt_control, pcr_ca2_control, pcr_ca1_interrupt_control :: Word8
pcr_cb2_control           = 0xe0
pcr_cb1_interrupt_control = 0x10
pcr_ca2_control           = 0x0E
pcr_ca1_interrupt_control = 0x01

-- pcr CB2 control bits
pcr_cb2_output_pulse, pcr_cb2_output_low, pcr_cb2_output_high :: Word8
pcr_cb2_output_pulse = 0xA0
pcr_cb2_output_low   = 0xC0
pcr_cb2_output_high  = 0xE0

-- pcr CB1 interrupt control bit
pcb_cb1_positive_int :: Word8
pcb_cb1_positive_int = 0x10

-- pcr ca2 control bits
pcr_ca2_output_pulse, pcr_ca2_output_low, pcr_ca2_output_high :: Word8
pcr_ca2_output_pulse = 0x0a
pcr_ca2_output_low   = 0x0c
pcr_ca2_output_high  = 0x0e

-- pcr CA1 interrupt control bit
pcb_ca1_positive_int :: Word8
pcb_ca1_positive_int = 0x01

ic32_keyboard_write :: Word8
ic32_keyboard_write = 0x08