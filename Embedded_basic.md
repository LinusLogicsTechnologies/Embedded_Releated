/* 
 * In embedded systems, the C programming language is most often the language of choice. For more intensive
 * elements in the system, assembly can be used. Embedded C is distinct from typical C programming in its
 * requirements for efficiency, its limited resources, and its unique hardware problems which are much less common in
 * the majority of C programs. Even still, the language itself is the same, so check out K&R's The C Programming
 * Language and other reference books. 
 *
 * Some of the problems central to embedded systems programming:
 *  - Memory management
 *  - Register access and manipulation
 *  - Peripheral access
 *  - Communication over serial and USB lines
 *  - Handling of interrupts
 *  - Sharing of data with programs and ISRs, typically requiring multithreading-like techniques like semaphores
 *  - Managing compilation to the assembly level
 *      - Cross-platform compilation
 *      - Startup scripts
 *      - Linker scripts
 *      - Avoiding compiler optimization pitfalls
 * 
 * And these apply only to 'bare-metal' programs - programs utilizing real-time OSs have other common issues to
 * consider. 
 */

/***** BOOK RECOMMENDATIONS ******/
// A first reference for all the topics talked about here is:
//  * Valano, "Embedded Microcomputer Systems - Real Time Interfacing" 3rd ed. (2011)
// This should be the go-to reference for microcontroller programming and interfacing. It's one of the best books 
// I've found for these topics. 
//  * Deitel, "Operating Systems"
// Embedded systems just about -all- use operating-system principles for scheduling tasks, managing memory, 
// handling mutual exclusion, creating unified hardware interfaces, and so on. This is a great book to have.
// Another good reference:
//  * Douglass, "Design Patterns for Embedded Systems in C" (2011)
// Featuring Group-Of-Four style design patterns applied to microcontroller systems, this is a neat book. 
// Next suggestion:
//  * White, "Making Embedded Systems"
// Good book for similar topics to Valano, and other essential topics like fixed-point arithmetic and bootloaders etc
// Some more:
//  * Warren, "Hacker's Delight"
// Basically -the- handbook of calculation and computing close to the metal, everything from bitwise operations 
// to optimized algorithms, a giant collection of low-level tricks and tips, a must-have.
//  * Clements, "Microprocessor Systems Design: 68000 Hardware, Software and Interfacing
// A book for the M68k architecture, which itself is interesting (if dated), which introduces a lot of concepts
// and is one I reach for to get a slightly different view of various topics like memory management. 

/********* PROJECTS OF NOTE **********/
// It's hard to find good projects using a microprocessor with open source firmware to demonstrate these principles.
// There are tons of projects out there, but finding high quality ones which aren't one-off tutorials or examples
// is tricky. Here's what I've found. 
/*
 
 * Dangerous Prototypes (http://dangerousprototypes.com/) A few different open source hardware projects, need more research
 * HackRF (https://github.com/mossmann/hackrf/wiki)   HackRF One uses an LPC43xx ARM microcontroller
    -> Uses OpenCM3 (https://github.com/libopencm3/libopencm3), an alternative microcontroller library
 * https://github.com/blacksphere/blackmagic          Black Magic Probe (BMP), JTAG interface and debugger
 *  -> Also uses OpenCM3
 * (https://hforsten.com/cheap-homemade-30-mhz-6-ghz-vector-network-analyzer.html) This is a VNA (!), uses HackRF firmware
 * OpenMV (https://github.com/openmv/openmv)          Machine vision, supports STM32 devices
 * Klipper (https://github.com/KevinOConnor/klipper)  3D printer firmware, compatible across many devices
 * Marlin (https://github.com/MarlinFirmware/Marlin)  Another 3D printer firmware project, broad support
 * NumWorks Epsilon (https://github.com/numworks/epsilon)  NumWorks is an open source graphing calculator with amazing
 *         documentation on everything from hardware to mechanical design to software. Runs bare metal ARM. See also the
 *         website, here: https://www.numworks.com/resources/engineering/software/
*/

/********* COMPILERS AND PREPROCESSOR STATEMENTS *********/
// Know your compiler. Assuming GCC, we can look at common preprocessor statements. These can really throw you
// for a loop when you see them in code unexpectedly, especially in manufacturer code, hardware drivers, and the
// CMSIS library. But don't worry, they're only here to help. 

// Some common reasons for needing preprocessor statements:
//  - Defining macros and constants
//  - Adding extra warnings
//  - Forcing the compiler to optimize or not optimize a section
//  - Forcing the compiler to make code inline
//  - Forcing the compiler to set a variable to a particular location in memory
//  - Inserting assembly directly into a C file
// Many statements seen frequently, such as #pragma once, are specific to a compiler (in that case, visual
// studio's C compiler). These may be shorthands for common statements, such as include guards. Others can be
// used to pass argument-like commands to the compiler. Some common and useful preprocessor statements follow.
// Note that when a compiler-specific feature is used, it's common practice to alias it. For example, the asm
// keyword, a C extension available to GCC, might not be available in other compilers. To increase portability,
// then, it's good practice to have something like this:
#ifdef __GNUC__
#define __asm__ asm 
#endif
#ifdef __MSC_VER
#define __asm__ <visual studio assembly keyword>
#endif
// etc


// Note how __GNUC__ is assumed to be defined whenever we use gcc. There are loads of these that can be used to
// check system and compiler information. For example,
// Architecture:
//      __i386__
//      __x86_64__
//      __arm__ (and __ARM_ARCH_5T__ or __ARM_ARCH_7A__)
//      __powerpc64__
//      __aarch64__
// Compiler:
//      _MSC_VER
//      __GNUC__
//      __clang__
//      __MINGW32__
//      __MINGW64__

// Common preprocessor statements: 

// 1.  __attribute__
// This statement covers a whole lot of different options for "attributes" of functions, variables, types,
// labels, enums, and statements. The syntax is:
//      __attribute__ ((<attribute list>))
// Note the double parentheses. The attribute list is a comma-separated sequence of attributes, which can be
// empty, an attribute name, an attribute name followed by a parenthesized list of parameters for the attribute.
// You're also allowed to put double underscores around an attribute name to avoid conflicts. 
//
// Let's look at a few attributes to get an idea of how they work. Attributes for variables, functions, etc can
// be organized into 'common' attributes and architecture-specific attributes. As an example, take the
// warn_if_not_aligned variable attribute. Given a struct, we may want to ensure data alignment to some number of
// byte boundaries. The __attribute__ section goes after the variable definition in this case.
struct reg_struct
{
    uint32_t section1;
    uint32_t section2 __attribute__(( warn_if_not_aligned(16) )); // Issue a warning if this section is not 
        // aligned to a 16 byte boundary
};
// Variable attributes include: 
//      aligned
//      aligned (alignment)
//      warn_if_not_aligned(alignment)
//      alloc_size(position)
//      alloc_size(position1,position2)
//      cleanup (cleanup_function)
//      section ("section name")            Set the section (e.g. .bss, .data, specialized sections)
// Function attributes include:
//      aligned
//      aligned (alignment)
//      always_inline
//      constructor
//      constructor (priority)
//      destructor
//      destructor (priority)
//      noreturn
//      section ("section name")
// ARM-specific function attributes:
//      general-regs-only   Indicate no specialized registers (floating-point or Advanced SIMD) should be used
//      interrupt   Indicate function is an interrupt handler. Can take a string argument "IRQ", "FIQ", "SWI",
//                  "ABORT", or "UNDEF" to specify the type of interrupt handler. The type is ignored for
//                  ARMv7-M.
//      isr         Alias for interrupt
//      target      Specify instruction set, architecture, floating-point unit for function
// For example:
void f () __attribute__ ((interrupt));


// 2.  asm (and __asm__)
// Assembly can be inserted inline with the following syntax:
//      asm [qualifiers] ( "assembly instructions" )
// You can also use extended asm, which lets you read and write C variables and to jump to C labels, when you're
// within a C function. You cannot use extended asm at file scope. 
//
// The qualifiers are either volatile or inline. However, volatile has no effect, as all asm blocks are volatile.
// The assembly instructions section is a literal string. For multi-line assembly, you can insert a newline (and
// preferaby a tab as well), '\n\t', then continue. 
// Example:
asm("MOV r3, 0x80\n");
// If you're inside a C function, you can use extended asm. It has a different format:
//      asm [qualifiers] ( "template"
//                       : <output operands>
//                       [: <input operands>]
//                       [: <clobbers>] )
// Or:
//      asm [qualifiers] ( "template"
//                       :
//                       : <input operands>
//                       : <clobbers>
//                       : <goto labels> )
//
// In extended asm, qualifiers available are volatile, inline, and goto. In this case, volatile is -not- assumed
// by default, so if the statements can change variable values, you should use volatile. The goto qualifier
// indicates that the statement may jump to one of the labels listed in <goto labels>. 
//
// Extended asm is more like a printf statement for assembly. The best source for this stuff is the gcc docs:
//  https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html
// So there.

// 3.  register
// The 'register' keyword is used to indicate that a particular variable is going to be used very often for a
// short time, so it should be kept in a register as opposed to in a stack frame.

/********* TYPES **********/
// We'll assume a 32-bit word size for the target platform. 

// Endianness refers to the stored order of bits or bytes for data. If the type is big-endian, that means the
// big-end (MSB) comes first (bit/byte 0) while the little-end (LSB) is last (last bit/byte). Little-endian is
// the opposite, the little end (LSB) comes first. 
//
// For example, in 8-bit binary, the number 122d is represented 0111 1010b. In little-endian binary, this is
// stored as:
//  7   6   5   4   3   2   1   0   BITS
//  0   1   1   1   1   0   1   0   VALUES
// While in big-endian binary, it would be flipped:
//  7   6   5   4   3   2   1   0   BITS
//  0   1   0   1   1   1   1   0   VALUES
//  
// It's more common for endianness to refer to bytes. In this case, the endianness refers to the
// byte-order. A byte can be stored in hex with two digits, e.g. FF or 32 or 1A. If we have the uint_16t type and
// we want to store the hex 0x1B0E, in little-endian we would store it as:
//  1   0   BYTES
//  1B  0E  VALUES
// Where the 'little end' is stored in the lowest location in memory. In big-endian, we would have:
//  1   0   BYTES
//  0E  1B  VALUES
// Where the 'big end' is stored in the lowest location in memory. 
//
// For whatever number of bytes is considered for one piece of stored data (2 bytes, 4 bytes, 8 bytes) the 
// bytes obey the ordering: e.g. a 32-bit type stored little endian with data 0xAB CD EF 01 is stored in memory
// as 01 EF CD AB. 
//
// Often in computing memory addresses are written from left-to-right with the lowest address value on the left.
// In this convention, 0xABCD would be written in little-endian as CD AB (little-end first) and in big-endian as
// AB CD (big-end first). In hardware, the convention tends to be flipped for writing bits (e.g. in a register). 
// In communication, the convention is that reading from left-to-right has the left side being the first
// received/first transmitted. So sending the data 12 34 means sending the byte 12 first, and the byte 34 last.
// If we have hex 0x1234, big-endian data transmission would be 12 34, while little-endian would be 34 12.

// For int types, prefer types with explicit sizes ("fixed-width")
// Unsigned ints
uint8_t var1;
uint16_t var2;
uint32_t var3;

// Signed ints
int8_t var5;
int16_t var6;
int32_t var7;

// For floating-point arithmetic we have float and double. However, many embedded systems don't support
// floating-point arithmetic in hardware, so the compiler can be told to use software-based floating-point
// (soft-fp). The software implementation can take up a fair amount of memory, so hardware FP is much better.
float var9; // 1 word (32-bit), called "single precision"
double var10; // 2 words (64-bit), called "double precision"

/* Single- and double-precision floating point formats are standardized in IEEE 754. A float ('binary32' in the
 * standard) is defined as 1 sign bit, 8 exponent bits, and 23 significand bits/fraction bits. The
 * float fraction, or significand, is the non-zero numerical part, which always appears after the decimal. The
 * expnent is the 10^x exponent. Thus all floats are stored as numbers between 0 and 1, with an exponent. For
 * example, the number 10.5 is + 0.105 x 10^2. The sign bit is the MSB (bit 31), the exponent is the next 8 bits
 * (bits 30 to 23 inclusive), and the fraction is the 23 LSB bits (bits 22 to 0). 
 *
 * In double-precision types ('binary64', or 'double'), the sign is 1 bit, the exponent 11 bits, and the fraction
 * 52 bits
 *
 */

/* EXAMPLE: Converting between floats and ints */
uint8_t num_uint = 128; // Stored as 1000 0000
int8_t num_signed = (int8_t)num_uint; // Now we have the special value -128 (1000 0000) which is 
// the "lowest negative number" whos 2's comp is itself

float num_f = (float)num_uint; // Now stored form changes a fair bit
// Let's investigate by converting the float to bytes using a number of different techniques.
// Version 1: union
union{
    float f;
    uint8_t bytes[4];
} u;
u.f = num_f;
// Now bytes is (from byte 0 to byte 3): 00 00 00 43

// Version 2: cast to byte array
uint8_t num_fbytes[4] = (uint8_t*) &num_f;
// Bytes is same as with union

// Both of the above depend on the endian-ness of the system. We can remove this dependency with the following.

// Version 3: cast to int and use bit-shifting and bitwise AND
uint32_t float_as_int = *(uint32_t*) &num_f;
uint8_t num_fbytes2[4];
for(int i = 0; i < 4; i++)
    num_fbytes2[i] = (float_as_int >> 8*i) & 0xFF;

// This takes the value byte-by-byte starting with the LSB and shifting to the MSB, it doesn't 
// depend on endian-ness. Output is still (byte 0 to byte 3): 00 00 00 43

// Now let's try interpreting the result. If our system were big-endian, then 00 00 00 43 would
// indicate a positive sign, exponent of 0 (2^0) and a fractional part of 67. This doesn't make
// much sense, because we expect 128. If it's little-endian, then the 43 (=0100 0011) is first, 
// so the sign is 0, and the next 8 bits (which includes the first bit of the next byte) is
// 43 << 1 (left-shift by 1) = 10000110 = 134. This is the exponent value. But to use it we must
// first subtract by 127 (why?) and we get 134-127 = 7, thus the result is 2^7 with a fractional
// part of 0 (-> 1.0), 2^7 = 128. 
// This shows that this x86 system is little-endian, which is expected. 

// FLOATING-POINT PROCESSING UNIT (FPU)
// An embedded FPU can be included in microcontroller architectures. These include instructions for
// floating-point arithmetic (+-*/), square roots, and more. Check your processors's technical reference manual as
// well as the processor architecture's architecture reference manual for information about your deivice. 

// FIXED-POINT ARITHMETIC
// Implementing floating-point arithmetic often involves a floating-point unit, either a hard FPU (like a
// secondary CPU) or a soft FPU (a software library). With a hard FPU, we can get away with floating-point at
// little computational cost. If on the other hand we only have a soft FPU, the overhead can be considerable. The
// fact is, the processor is performing integer arithmetic, and floating-point operations require many
// instructions. 
//
// To help the situation, one can use fixed-point arithmetic. With fixed-point, integers are used which can be
// separated into an integer part and a fractional part, by breaking e.g. 32 bits in two pieces. 
//
// To successfully use fixed-point, the precision (number of fractional bits) and data width (total number of
// bits) must be specified. When an operation is performed, one must know the precision and width of the
// two operands before calculating. Overflow and underflow errors can also crop up, and these should be checked. 

// Example implementation (from here: https://www.embedded.com/fixed-point-math-in-c/):
// A 12.20 fixed-point number has a range of [0.0 - 4095.1048575], or [0 - (2^12 -1).(2^20 -1) ]
typedef union {
    int32_t full_number;
    struct {
        int32_t integer : 12; // 12 bit integer part. The colon (:) indicates a bit-field, here 12 bits
        int32_t frac    : 20; // 20 bit fractional part
    } part;
} Fixed12_20; // Fixed point, 12 bit integer part, 20 bit fractional part. 

// This can be printed to std out:
void printfx(Fixed12_20 num)
{
    printf("%04d.%06d\n",num.part.integer,num.part.frac); // 4 digits (to 4096) and 6 digits (to 1048576) with 
    // zero padding
}

// This representation does not lend itself easily to generalization, meaning it's hard to support arbitrary
// widths of integer and fractional parts.
// Instead, we can use bit shifting. This is equivalent to multiplying by powers of two, which some people call
// the multiplying factor. But most prefer the concept of bit-shifting. 
// Here's another example implementation, slightly different:
#define FixedT  int32_t
#define FIXED_PREC     20
#define FIXED_INTPART(A)    A>>FIXED_PREC
#define FIXED_FRACPART(A)   A & ((1<<FIXED_PREC)-1) // 0xFFF...F
const uint8_t N_DEC_DIGITS[] = {0,1,1,1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7,7,7,8,8,8,9,9,9,10,10,10};

FixedT FIXED_CREATE(int16_t integer, uint16_t frac)
{
    return ((FixedT)(integer)<<FIXED_PREC)+(FixedT)(frac);
}

void FIXED_PRINT(int32_t* Ap)
{
    printf("%0*d.%0*d",N_DEC_DIGITS[32-FIXED_PREC],FIXED_INTPART(*Ap),N_DEC_DIGITS[FIXED_PREC],FIXED_FRACPART(*Ap));
}

int main()
{
    FixedT a,b;
    a = FIXED_CREATE(12,34);
    b = FIXED_CREATE(45,67);
    FIXED_PRINT(&a);
    printf("\n");
    FIXED_PRINT(&b);
    printf("\n");
}


// CASTING AND COPYING MEMORY
// It is often the case that we receive data e.g. as a byte array, but we want to cast that same
// byte data into a data type like int or float or even a string. We've seen how to convert data
// into bytes using e.g. unions, and the same can be done again. We can also use bit arithmetic
// with shifting, AND, OR, XOR, etc. 
//
// For arrays, the C library provides memcpy (declared in string.h). The memcpy function takes a
// pointer to a destination array (see next section) and a source array and a length argument and 
// copies the bytes from the source array into the destination. 
// The template is:
//  memcpy(void* to, const void* from, size_t num_bytes)

uint8_t byte_array[5] = {0xFF,0x00,0x12,0xA2,0x09};
uint8_t blank_array[5];
memcpy(blank_array, byte_array,5);

// Note that a void* pointer in C is a pointer without a specified type, so it can be cast to any
// other pointer type to allow quick conversion of data. We can send any type of array to memcpy
// as long as the number of bytes (not the number of elements) matches up. Memcpy is actually a 
// very simple function, which can be implemented without using the whole C standard library, 
// as follows (from the C library):

void* memcpy(void* dst, const void* src, size_t len)
{
   size_t i;

   /*
    * memcpy does not support overlapping buffers, so always do it
    * forwards. (Don't change this without adjusting memmove.)
    *
    * For speedy copying, optimize the common case where both pointers
    * and the length are word-aligned, and copy word-at-a-time instead
    * of byte-at-a-time. Otherwise, copy by bytes.
    *
    * The alignment logic below should be portable. We rely on
    * the compiler to be reasonably intelligent about optimizing
    * the divides and modulos out. Fortunately, it is.
    */

    if((uintptr_t)dst % sizeof(long) == 0 &&
       (uintptr_t)src % sizeof(long) == 0 &&
       len % sizeof(long) == 0) {

        long* d = dst;
        const long* s = src;
        
        for (i = 0; i<len/sizeof(long);i++){
            d[i] = s[i];
        }
    }

    else {
        char* d = dst;
        const char* s = src;
        
        for(i=0; i<len; i++) {
            d[i] = s[i];
        }
    }

    return dst;
}

// This is essentially just copying bytes in a loop. It has the added bonus of also checking for word alignment
// to speed up the copying. 

/******** MATH ********/
// It is often the case that we must perform math on integer or float or fixed-point data. What options do we
// have in an embedded system? 

// There must be a distinction between integer, fixed-point, and floating-point math. For the basic operations:
// +    Addition, built-in
// -    Subtraction, built-in
// *    Multiplication, built-in
// /    Division, built-in; for integers the fractional part is discarded (sometimes called round-towards-zero)
// %    Modulus, built-in for integers
// pow  Exponentiation, pow() in math.h for double, float, and long double. For integer and fixed-point 
//      exponentiation, without casting, not built-in; see below. 
// sqrt Square root, sqrt() in math.h for double, float, and long double. For integer and fixed-point, not
//      built-in; see below.
// 
// If you're using floating-point math (float, double) you can use the math.h header file, part of the C standard
// library. Many other functions are available, including trig, powers, exponentials, logs, and rounding
// functions. Optimized routines are provided by Arm for Arm-core devices, found here:
// https://github.com/ARM-software/optimized-routines
//
// For integer and floating-point math, when floats and doubles are to be avoided, there are a few options.
// First, you can provide your own implementations. This is essentially algorithm design, so if you're going for
// highly optimal code (optimal size, or memory usage, or speed, or code maintainability) best not to trust
// yourself to have the best algorithm. Libraries do exist, for example liquid-fpm (floating-point math library,
// merged to liquid-dsp) for floating-point math is quite complete. 
//
// Some algorithms and topics to look at when rolling your own math functions:
//  Integer square root
//      Digit-by-digit algorithm
//      Approximation methods
//  Exponentiation
//      Exponentiation by squaring
//      Addition-chain exponentiation
//  Logarithms
//      CORDIC
//      Taylor approximation
//      Table lookup and interpolation
//      Turner's algorithm (IEEE 2011)
//  Trig and hyperbolic trig
//      CORDIC
// An approach you will often find is to define convenient exponential and log functions (such as e^x and log2)
// and to use exponent and log rules to calculate with other bases. Iterative numerical methods (e.g. 
// Newton-Raphson) are common, albeit with variable-time. 



/********* ACCESSING MEMORY *********/
// Registers, SRAM, Flash, we often need to access memory. How can we do it? It's straightforward in most cases.
// We have addresses in the form of pointers, whose addresses are set manually (as opposed to specifying the
// address of a variable).

uint32_t var1 = 10;         // Typical way of setting pointer addresses
uint32_t* var_ptr = &var1;  //

uint32_t* reg_ptr = (uint32_t* )0xE000E000; // Setting a pointer to a specific address in memory (e.g. a register)
// Note that we must cast the address to a pointer type

// Many times, we need to access multiple words, so arrays are used. A quick refresher about the finer points of
// C arrays is in order. 
uint32_t x[10]; // Declare an array of 10 words
uint32_t* y[10]; // Declare an array of pointers to words

x[0];   // Access first value
&x[0];  // Get memory address of first value
&x;     // Same as &x[0]

uint32_t* z = &x;   // Declare a pointer to the first element of x

// Now we can access the pointer z as an array
z[0];   // First element of x

// But note that this is a convenience, z is still a pointer. The pointer uses indrect addressing, while the
// array uses direct addressing, meaning z[0] accesses the address of x[0] and returns its istored value, while 
// x[0] returns the value at that location directly. 

// Arrays cannot be assigned addresses like pointers can. To get a location in memory, we need to assign a
// pointer to that address, and an array can only be used through a pointer. But instead, we can use pointer
// arithmetic and access brackets, avoiding arrays for the most part.
uint32_t* var1_ptr = &var1;
var1_ptr[0];    // Equivalent to *( var1_ptr + 0)
var1_ptr[12];   // Equivalent to *( var1_ptr + 12)
// Note there's no bound checking here, so we can access whatever memory starting from var1_ptr. Also, the square
// brackets indicate dereferencing. 

// When memory is divided into logical sections, we can use structs of arrays to reserve the memory. This can be
// more convenient than using pointer arithmetic directly. Recall that in C structs must be prefaced with
// "struct" before its type name in all occurrences, so it's often typedef'd.
typedef struct{
    uint32_t section1[4];
    uint32_t reserved[10];
    uint32_t section2[4];
} MEMORY_type;
// This has two parts: the typedef, of the form "typedef <expression> <typename>" and the struct definition,
// "struct { <data > }". Now to use this at a particular memory location, we need a pointer that accepts an
// address, and we can use the MEMORY_type as the pointer type.
MEMORY_type* memory_accessor = (MEMORY_type*)0xE000E000; // Same as before, set an address for the pointer
// Alternatively, we can use a uint32_t pointer, call it memory_ptr, and then remember how to access the data,
// with the different offsets (possibly using #defines) but this is more work.


// Dynamic Memory
//
// In a simple world, memory would be static, with local variables stored on the stack and global variables
// stored in .data. But we also have dynamically allocated data, stored on the heap, and sometimes this can be
// tricky. 
//
// Dynamic memory itself is simple enough, and I assume we're all on the same page as to what it means. In C, we
// use malloc() to allocate dynamic memory, and free() to free that memory. Under the hood, malloc() uses a
// function called sbrk() to resize the heap data segment. (brk = break value, sbrk = space? brk, probably).
//
// Some references:
// https://web.archive.org/web/20190214041636/http://fun-tech.se/stm32/linker/index.php
// http://e2e.ti.com/support/archive/stellaris_arm/f/471/t/44452
// https://stackoverflow.com/questions/10467244/using-newlibs-malloc-in-an-arm-cortex-m3
//
// Most of the trouble with dynamic memory comes from issues with the linker script. We won't cover that here. 


// Accessing Registers
//
// In assembly, we can access registers by writing them in commands, like mov. In C, we can only directly access
// "memory-mapped", meaning they have a memory address associated with them. If that's the case, we can access
// the register by setting a pointer to it, specifying the address directly. 
//
// Other registers must be accessed indirectly. These are accessed by "helper registers" called indirect
// registers; these include set registers and clear registers. If a register is not writable directly,
// you can use a set register to set bits within that register, and a clear register to clear bits within that
// register. The indirect registers are mapped bit-by-bit, so you set bits in the set register to set bits in the
// target register, and you set bits in the clear register to clear bits in the target register. 

// The manufacturer should provide a header file, e.g. for ARM devices, in a CMSIS Device driver folder. These 
// tend to be large files with memory mappings for all peripherals.

// When defining registers on a bit-by-bit basis, you don't have to use bitwise operators all the time. You can
// use bit fields in structs:
typedef union{
    uint32_t full_register;
    struct{
        uint32_t RWn    : 1;    // Bit 0
        uint32_t BufEmpty : 1;  // Bit 1
        uint32_t Data : 8;      // Bits 10 : 2
        uint32_t Addr : 8;      // Bits 18 : 11
        uint32_t Reserved : 14; // Bits 32 : 19
    } fields;
} StatusRegister;

// The bit fields specify the number of bits used for the given label. The bits are occupied in order. All the
// struct data members have the same type, and they can be accessed:
StatusRegister sr;
sr.fields.RWn = 1;
sr.fields.BufEmpty = 0;
sr.fields.Data = 0b11001100;
sr.fields.Addr = 0b11111110;
// Result is: sr.full_register => 00000000 00000011 11111011 00110001 (260913 in decimal)


/********* GLOBAL (EXTERNAL) VARIABLES *********/
// External (global) variables are defined at file scope
uint8_t glob;

// Now we can use it. If the definition is in the same file, we can use it directly:
void glob_init()
{
    glob = 0;
}
// If it's in another file, we need to redeclare it with 'extern'
void glob_inc()
{
    extern uint8_t glob;
    glob++;
}

// A variable that is global might need to be accessed in another stack (notably during interrupts). If this is
// the case, it is very important that you use the -volatile- keyword. Volatile tells the compiler not to
// optimize out the variable, because it can change at any time, from anywhere. 

volatile uint8_t interrupt_ctr;
void handle_init_interrupt() // An ISR
{
    interrupt_ctr = 0;
}
void ctr_inc()
{
    interrupt_ctr++;
}

// In general, volatile should be used when the compiler might think a variable isn't doing anything. This
// includes for loops with no body, temporary variables used for debugging values, and interrupt routines, as
// well as for peripheral registers, whose values can change without the main program being aware.

// * Shared Memory, Mutex, Locks and Semaphores *
// In multithreading and multi-stack applications, we have a shared resource problem. Memory is shared between
// processes (such as the main loop and various interrupts) and during various operations (termed 'critical
// sections') only one process should have control of the data at a time. We don't want to have data changing
// while we're transmitting the contents of an array. 
//
// To solve this problem requires -mutual exclusion-, meaning only one process is in its critical section at a
// time. We also require that the system won't -deadlock- e.g. when two processes try to take control at the same
// time. Mutual exclusion is abbreviated -mutex-. This can be achieved a few ways.
//
// Option 1. Disable interrupts
// If we have a single processor core, and interrupts which can access data, then the simplest solution is for
// critical sections to disable interrupts. An operation that cannot be interrupted is called -atomic-. 
// Here's an example, for global data tx_data which has a length LEN_DATA, and disable_interrupts() and
// enable_interrupts() assumed to do what they say.

void send_data()
{
    disable_interrupts();
    for(int i = 0; i < LEN_DATA; i++)
    {
        send(tx_data[i]);
    }
    enable_interrupts();
}

// This is good. It has the following issues. If the critical section is long, we can have issues with watchdogs,
// and clock synchronization, which depend on regular interrupts. If the program halts during the critical
// section, then the system is stuck, and must be forcefully reset. 

// Option 2. Busy waiting
// In busy-waiting, a process repeatedly checks to see whether a variable is locked. There are instructions which
// work with this system, although in general this sort of 'polling' is frowned upon. The instructions include:
//  - test-and-set
//  - compare-and-swap
//  - fetch-and-add
//  - load-link/store-conditional
// These operations are all atomic, so they cannot be interrupted. 

// Option 3. Software solutions
// We can avoid changing data using software alone. A simple example would be to store the current value of some
// variable, and then to check that the two values are equal.

volatile uint8_t timer_ctr;

void interrupt_handler()
{
    timer_ctr++;
}

void send_timer_count()
{
    uint8_t temp_timer_ctr = timer_ctr;
    while(temp_timer_ctr != timer_ctr)
        temp_timer_ctr = timer_ctr;

    send(timer_ctr);
}

// The above uses a loop to find when the two values are the same. This has obvious issues, but it can be useful.
// Another option is to use a lock variable, often called a mutex (mutual exclusion).
volatile uint8_t timer_ctr;
volatile bool timer_ctr_lock;

void interrupt_handler()
{
    if(!timer_ctr_lock)
        timer_ctr++;
}

void send_timer_count()
{
    if(!timer_ctr_lock)
        timer_ctr_lock = true;
    send(timer_ctr);

    timer_ctr_lock = false;
}

// This uses a volatile bool to check when the variable timer_ctr is available. This has its drawbacks, namely
// that counter increments are simply skipped, instead of waiting until it's available. Counts are lost. (Note
// that the bool type may not be defined.)

// This brings up the problem of ensuring that every process will, eventually, run. Another technique is to have
// two variables, and a lock, and to have the interrupt set one variable or the other depending on the lock. 
volatile uint8_t timer_ctr
volatile uint8_t timer_ctr_2;
volatile bool timer_ctr_lock;

void interrupt_handler()
{
    if(!timer_ctr_lock)
        timer_ctr++; // Increment if unlocked
    else
    {
        timer_ctr_2 = timer_ctr; // Otherwise increment the other counter
        timer_ctr_2++;
    }
}
void send_timer_count()
{
    if(!timer_ctr_lock)
        timer_ctr_lock = true; // Confirm the lock

    send(timer_ctr); // Critical section

    timer_ctr_lock = false;

    if(timer_ctr != timer_ctr_2) // Update in case the other variable changed
        timer_ctr = timer_ctr_2;
}


// STATIC VARIABLES - Better Global and Local Variables
// There are two types of variables that are closely related to globals. Both are declared "static", but in
// different contexts. 
//
// The first is a variable at file scope declared static. 
static uint8_t my_var;
// This variable is global within the file, but ONLY within the file it's declared in. It cannot be used outside
// of THIS file. 
//
// The second is a local variable in a function declared static.
void func()
{
    static uint8_t ctr = 0;
    ctr++;
}
// This variable will only be initialized the FIRST time the function runs. After that, the initialization (ctr =
// 0) is skipped, as the static local variable keeps its value on each run. In this example, the first time
// func() is called, ctr is initialized to 0, then gets incremented to 1. The second time we call func(), the
// initialization is skipped, ctr is still 1, and we increment ctr again to 2. The local variable keeps its value
// between function calls, unlike normal local variables. 
//
// Static global variables give us a way to have variables shared between functions without having those
// variables be truly global. Static local variables allow us to have a function maintain its own set of
// variables as if it were a data structure. These are powerful tools that allow us to control variable scope and
// promote good encapsulation. 
//
// NOTE: In both cases, static variables have limited scope. They cannot be referenced outside of their
// respective scopes. But something that's rather nice is, -pointers- to static variables are safe. This means
// you can declare a global static variable, and provide a pointer to that variable, and the pointer can be used
// anywhere in the program. 



/*********** SHARING DATA WITH INTERRUPTS ***********/
// Most programmers don't like the idea of using globals. It's been instilled in us that globals are evil, they
// lead to messy and fragile code, they pollute the namespace, they're evil. 
//
// Put these fears aside. Keep an eye on your globals, but for small embedded programs, it's not a concern. Even
// for larger programs, there are tons of reasons to use global variables. Just ask, is avoiding a global worth
// the added overhead in code and maintenance? 
//
// That being said, consider the case where an interrupt will generate a lot of data, and our main program needs
// to detect the availability of this new data. What's the best way to do this? The first thing that comes to
// mind is flags. 
//
// The only way for the main loop to "detect" the calling of another function like an ISR while looping is 
// (a) if a volatile variable has changed value and (b) the main loop checks if that variable has changed. This 
// is the purpose of a flag variable, a bool or int value that the main loop checks repeatedly. 
//
// Here are some methods of achieving this. 

// 1. Global variables, main loop checks flag and resets it
// This option is suitable for smaller programs with only a few interrupts, or for really common and widely
// shared data

volatile uint8_t data_avail = 0;    // Flag for available data
volatile uint8_t b_data[255];       // Byte array that will hold the data
int main()
{
    while(1)
    {
        if(data_avail)
        {
            /* Do something with b_data */

            // Reset
            data_avail = 0; 
        }
    }
    return 0;
}

void isr() // Called automatically when new data comes in
{
    if(!data_avail)
    {
        b_data = sys_get_data(b_data,255);  // sys_get_data would be a function that loads b_data with the data 
                                            // that caused the interrupt
        
        // Set the flag
        data_avail = 1;

    }
    else // data_avail == 1
    {
        // The main loop hasn't read the old data yet, handle this case somehow
    }
}



// 2. "Driver" functions and data in a separate file, using static globals, and a function interface
// This is suitable for larger programs, where we trade a little extra overhead for more robust and maintainable
// software

/* it_driver.h */
// Prototypes for functions, defining data structures, typedefs, #defines, etc
// e.g.
uint8_t data_avail();       // Return 1 if new data is available, 0 otherwise
void get_byte_data(uint8_t* bytes, uint32_t len);   // Load 'len' bytes of data into 'bytes', reset new_data
// etc

/* it_driver.c */
// Declaring static globals, providing implementations for functions
static volatile uint8_t new_data = 0; // Flag for whether there is new data
static volatile uint8_t b_data[255];  // Byte array with max size of 255 bytes
void isr() // Called automatically/asynchronously when new data arrives
{
    if(!new_data)
    {
        b_data = sys_get_data(b_data,255);
        new_data = 1;
    }
    else 
    {
        // Main loop has read old data yet, ...
    }
}
// Other functions defined too


/* main.c */
#include "it_driver.h"
int main()
{
    uint8_t b_data[255];
    
    while(1)
    {
        if(data_avail()) // Some better naming is of course in order
        {
            get_byte_data(b_data,255);
        }
    }
}


// 3. Same as 2, but using handler variables
// We often have multiple peripherals which have the same methods available to them, e.g. multiple I2C lines. We
// don't want to have to give each I2C line its own driver when the drivers will all be identical. To get around
// this, we define the interface as in method 2, but each function will take a "handler" parameter referring to a
// particular instance of the peripheral, and it will operate on that handler instead of global static data. 
// The handler variables themselves will be declared in the driver source file, so any time we want to add or
// remove an instance we'll need to change them in that source file. 
//
// The handler variable is a data structure (a struct) which stores data identifying a particular instance of
// the peripheral. This is often a true global variable, or a pointer to a global variable, defined in the driver 
// source (.c) file. 

/* it_driver.h */
typedef struct{
    uint8_t b_data[255];
    // etc
} it_handler_struct; // Create a struct, call it it_handler_struct

/* it_driver.c */
it_handler_struct it_handler; // Global handler variable, used by main program

// If we want to instead provide a pointer to the data handler, we may want to have the user pass that pointer
// around, but -not- dereference it. They can have it_handler_struct* it_handler_ptr, but they should not be
// using it like it_handler_ptr->b_data, or *it_handler_ptr. This is cleaner. To promote this sort of behavior
// and interaction, we can typedef the pointer to the struct to its own handler type, making the struct pointer
// not look like a pointer, as a reminder of the design intent. 

/* it_driver.h */
typedef struct{
    uint8_t b_data[255];
    // etc
} it_handler_struct;

typedef (it_handler_struct*) it_handler_type;

// Now the variable it_handler can be a type it_handler_type, which is actually a pointer, but the fact that it's
// a "type" expresses our design intent. 

// All the functions previously defined now need to be changed to accept a handler, and we're done. The main loop
// can specify which peripheral it's looking at by simply using the appropriate handler variable.



/*********** DATA STRUCTURES ***********/
// Common data structures include:
//  - Abstract data types
//      - Containers
//      - Lists
//      - Tuples
//      - Multimaps
//      - Sets
//      - Multisets
//      - Stacks
//      - Queues
//      - Graphs
//  - Linear data structures
//      - Arrays
//      - Linked lists
//      - Double-linked lists
//      - Circular buffers (aka circular queue, ring buffer, cyclic buffer, FIFO)
//  - Trees
//      - Binary trees
//      - B-trees
//      - Heaps
// Of these, particularly important in embedded systems are circular buffers, queues, stacks, and of course
// arrays. 

// ARRAYS
// Arrays are used to store contiguous blocks of data in memory. They are best used for fixed-length data, such
// as read-only data, data packets, and registers. 

// To declare an array:
uint8_t rx_msg[8]; // Declare an 8 byte array by size
uint8_t tx_msg[] = { 0x10, 0xff, 0x00, 0x10, 0xff }; // Declare an array by initializing elements
uint8_t tx_rx_array[8] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00} ; // Both size and initializing elems

// The length must be const data. You cannot use a non-const variable to specify array length. Arrays cannot
// directly be resized. Copying of data from one array to the other is done with e.g. memcpy (or a simple
// for-loop) as mentioned above.

// CIRCULAR BUFFERS
// Circular buffers are first-in-first-out (FIFO) data structures. The buffer is fixed-length, and you always
// read data from the oldest going to the newest. When the end of the buffer is reached, we start overwriting the
// beginning, hence the name "circular buffer." When this happens, the old data is lost, unless it has already
// been read. 
//
// Instead of specifying a 'start' and 'end' for the buffer, we use a 'head' and a 'tail', where the head is
// the most recent element and the tail is the next element to be read. If the head overtakes the tail, we have
// overwriting, which is typically not allowed, and may call for reallocation. The data itself is stored in
// memory as an array, which does have a start and end, and this must be handled in the implementation, but the
// user only knows the head, tail, and size. 

// Here's an example implementation. The header file:
#ifndef CIRCBUF_H
#define CIRCBUF_H 

#include <stdint.h>

// Header for circular buffer data structure

typedef struct{
    uint32_t head;  // Next free space
    uint32_t tail;  // Current element, to be popped
    uint32_t capacity; // Max buffer size
    uint8_t full;   // Whether or not the buffer is full (0 or 1)
    uint8_t* data;  // Actual buffer
} circbuf;

// Make a typedef for a handler to a circbuf pointer, so users don't think they're supposed to dereference it
typedef circbuf* circbuf_handler;
typedef uint8_t circbuf_status;

#define CIRCBUF_STATUS_OK           0
#define CIRCBUF_STATUS_FULL         1
#define CIRCBUF_STATUS_EMPTY        2
#define CIRCBUF_STATUS_WARN         3
// Warn = last byte written, now buffer is full

// Implementation functions
circbuf_handler init_circbuf(uint8_t* buf, uint32_t size); // Initialize circular buffer with buffer and size
void circbuf_reset(); // Clear buffer, set head = tail
circbuf_status circbuf_push(uint8_t* wbyte); // Push len bytes to buffer, return status
circbuf_status circbuf_pop(uint8_t* rbyte); // Pop len bytes from buffer to rbyte, return status
circbuf_status circbuf_get_status(); // Return a status value
uint32_t circbuf_get_capacity(); // Return capacity (full size) of buffer
uint32_t circbuf_get_size(); // Return number of elements in buffer
uint32_t circbuf_get_free_space(); // Return number of free spaces

void circbuf_print(); // Print circular buffer contents

#endif /* CIRCBUF_H */

// The source file: Note how we've hidden some of the implementation details by typedef-ing a pointer to the
// buffer object, and we also instantiate a buffer object in the source file. This could have been static, so
// that the user can't access it, but this is fine too. 

#include "circbuf.h"
#include <stdio.h>

circbuf circular_buffer; // Initialize circular buffer


circbuf_handler init_circbuf(uint8_t* buf, uint32_t size)
{
    circular_buffer.data = buf;
    circular_buffer.capacity = size;
    return &circular_buffer;
}

void circbuf_reset()
{
    uint32_t i;
    for(i=0;i<circular_buffer.capacity;i++)
        circular_buffer.data[i] = 0;
    circular_buffer.head = 0;
    circular_buffer.tail = 0;
}

circbuf_status circbuf_push(uint8_t* wbyte)
{
    // Need to determine whether or not the next space is available, 
    // updating if we go over the capacity
    uint32_t next_head = (circular_buffer.head+1) % circular_buffer.capacity;
    if(circular_buffer.full)
    {
        return CIRCBUF_STATUS_FULL;
    }
    else if(next_head == circular_buffer.tail)
    { // If we're going to overlap the tail next step (overflow)
        circular_buffer.full = 1;
        circular_buffer.data[circular_buffer.head] = *wbyte;
        circular_buffer.head = next_head;
        return CIRCBUF_STATUS_WARN;
    }
    else
    { // We aren't overlapping the tail (no overflow condition)
        circular_buffer.full = 0;
        circular_buffer.data[circular_buffer.head] = *wbyte;
        circular_buffer.head = next_head;
        return CIRCBUF_STATUS_OK;
    }
}

circbuf_status circbuf_pop(uint8_t* rbyte)
{
    uint32_t next_tail = (circular_buffer.tail+1) % circular_buffer.capacity;
    if(circular_buffer.tail == circular_buffer.head && !circular_buffer.full)
    { // Tail is caught up to head (empty condition)
        return CIRCBUF_STATUS_EMPTY;
    }
    else
    {
        *rbyte = circular_buffer.data[circular_buffer.tail];
        circular_buffer.tail = next_tail;
        if(circular_buffer.full)
        {
            circular_buffer.full = 0;
        }
        return CIRCBUF_STATUS_OK;
    }
}

circbuf_status circbuf_get_status()
{
    if(circular_buffer.tail == circular_buffer.head)
    {
        return CIRCBUF_STATUS_FULL; 
    }
    else if( (circular_buffer.tail+1)%circular_buffer.capacity == circular_buffer.head )
    {
        return CIRCBUF_STATUS_EMPTY;
    }
    else
    {
        return CIRCBUF_STATUS_OK;
    }
}

uint32_t circbuf_get_capacity()
{
    return circular_buffer.capacity;
}

uint32_t circbuf_get_size()
{
    return 0; // TODO
}

uint32_t circbuf_get_free_space()
{
    return circular_buffer.capacity - circbuf_get_size();
}

void circbuf_print()
{
    uint32_t i;
    printf("CIRCULAR BUFFER CONTENTS\n");
    for(i=0; i<circular_buffer.capacity; i++)
    {
        printf("0x%.2x\t",circular_buffer.data[i]);
    }
    printf("\n");
    for(i=0; i<circular_buffer.capacity; i++)
    {
        if(i == circular_buffer.head && i != circular_buffer.tail)
            printf("H\t");
        else if(i == circular_buffer.tail && i != circular_buffer.head)
            printf("T\t");
        else if(i == circular_buffer.tail && i == circular_buffer.head)
            printf("T/H\t");
        else
            printf("\t");
    }
    printf("\n");
    
}


// This sort of implementation should be enough to help implementing other data structures, where the general
// principles remain mostly the same. 



/*********** COMMUNICATION ************/
// The topic of "communication" is big, but there are many things shared across most common systems that
// can be discussed. 

// Device Communication:
//  - SPI
//  - UART
//  - I2C
//  - USB
//  - RS-232
//  - Microwire
//  - JTAG (not a communication protocol, but very common and somewhat related)

// Now we can apply the topics covered so far to the task of receiving and transmitting data over 
// one of these communication links. Some things to look for:
//  - Data, status, control, and other registers
//  - Interrupts
//  - Master/slave roles and responsibilities
//  - Speed
//  - Addressing scheme

// In embedded systems, a protocol like I2C can be implemented a few different ways. The communication is 
// always the same, but how we are required to interact with it varies. For example, if no I2C peripheral exists,
// we can program the protocol directly into a set of IO pins, a process called 'bit banging'. (Specifically,
// bit banging refers to any method of transferring data using software instead of dedicated hardware.)
//
// One level up from this, our microprocessor might have dedicated hardware for handling communications. Our role
// as the programmer is then to use the associated registers and interrupts to control when and how communication
// proceeds. To transmit data, we load it into a data register; to receive data, we read from a data register. 
//
// At a higher level of abstraction, we have high-level libraries such as HAL for STM32 devices which can provide
// functions for handling all of the common tasks required. 

// For any protocol (I2C, SPI, etc) that has associated peripherals, look for the following:
//      Control registers, for configuration of the peripheral
//      Status register, used for flags such as busy, overrun, buffer empty, buffer not empty, etc
//      Data register, data received or transmitted
// The protocols should support both software polling as well as interrupt modes. Software polling means you
// check the status register regularly to see if any new flags are set. Interrupt mode means an interrupt will be
// automatically called when a flag is set. Polling mode is also called blocking mode. Interrupt mode can use the
// interrupts to trigger DMA transfers instead of triggering an interrupt handler. 

// TRANSMITTING
// For transmitting, check the status register for a 'transmit buffer empty' flag. If this flag is a 0, there is
// still data being transmitted, so don't transmit. If it's a 1, begin a transmit sequence. This can be as simple
// as loading a byte into the transmit buffer. For managing transmitted data, you can either manually write
// words, or you can use a circular buffer to transmit its contents over a number of steps. 

// RECEIVING IN POLLING MODE (BLOCKING MODE)
// It is the user's responsibility to check the status register for changes. This can be simple, reading one
// word at a time:
uint16_t rx_word = 0;
while(1)
{
    if(SR->RXNE) // Status Register RX buffer Not Empty
    {
        rx_word = *DR; // Set rx_word to Data Register
        // Do something with rx_word
    }
}
// This has the obvious drawback that you can only read 16 bits at a time, and you need to use those bits right
// away. An alternative is to load the data register into a circular buffer, and handle the data as needed,
// processing in bulk or as fast as possible. 

// RECEIVING IN INTERRUPT MODE (NON-BLOCKING MODE)
// Create an interrupt handler (the symbol name is probably defined in the startup file). In the handler, first
// (as ALWAYS with interrupts) clear the interrupt. Then store the data any way that's convenient, using the
// methods described above (sharing data with interrupts, using global variables, etc). For example, load the
// data register into a circular buffer, and when the receiving operation is complete (e.g. after a string has
// terminated) use a flag variable to signal to the main() function that the data is ready. You could also handle
// the communication without using the main() loop at all, instead doing everything in an event-driven way. 

/******* PARALLELISM AND CONCURRENCY *******/
// You have an embedded system. There are 17 sensors, 25 status LEDs, a half dozen peripherals communicating 
// over three different communication protocols, a TFT display to update, and a USB connection to a host 
// computer. Where do you start? Maybe if it was 2 sensors, 5 LEDs, and a VCP connection you could do something
// like this:
int main()
{
    init(); // Initialize system
    while(1)
    {
        data1 = check_sensor_1();
        if(data1 > LIMIT)
        {
            set_pin(OVERLIMIT_PIN, 1);
        }
        serial_transmit(data1);
        
        data2 = check_sensor_2();
        if(data2 < UNDERFLOW)
        {
            set_pin(UNDERFLOW_PIN, 1);
        }
        serial_transmit(data2);
    }
}
// Here, in a function remniscent of Arduino programs, we have a main loop which runs through a 'checklist' of
// items, polling for updates and sending data out over serial. Obviously, this is not going to scale well.
// Instead, we want to handle multiple tasks -simultaneously-. 

// Operations can run nearly simultaneously in the following ways:
//      Parallel - Operations performed in true parallel, instructions executed at the same time
//      Concurrent - Operations performed in a round-robin fashion, allocating time to processes, processes take
//          turns executing instructions
// Managing tasks with finite resources, that's the problem we face. Sometimes we have four or eight CPU
// cores that we can use to achieve true parallelism. Other times, we need to prioritize certain tasks, allowing
// some tasks to run uninterrupted, while others are run when nothing else is going on. 

// Often, managing tasks is the objective of an -operating system-. If you need to guarantee that certain tasks
// will complete in a bounded amount of time, you would probably look for a real-time operating system (RTOS). If
// that's too much overhead, you can implement simple concurrency without much effort at all, like the above main
// loop example. 

// Some specific examples of parallel and concurrent processing:
//  Parallel
//      Multithreading, multiprocessing
//      Hardware (logic gates, FPGA)
//      GPU shaders
//  Concurrent
//      Schedule-driven processing
//      Event-driven processing
// You'll often find the following terms used in these contexts (though note that the meanings of these words
// change with different contexts):
//      Event
//      Task
//      Job
//      Process
//      Thread
//      Worker
//      Coroutine
//      Interrupt
// Try looking these terms up and seeing what differences there are between them in different contexts. For
// example, Linux uses processes, jobs, and threads; Windows has a task manager which handles programs and
// processes. 

// INTERRUPTS
// The first example of a concurrent system is already familiar: interrupts. Interrupts use priority, they can be
// nested, they sometimes use polling and otherwise are vectored (using ISRs). Some practical bits of advice
// about using interrupts that relate to concurrent systems:
//      ISRs should be as short as possible to avoid conflicts with other interrupts. Come in clean, manage the
//      interrupt, and return.
//      Avoid using iterative loops, busy-waits, and other time-consuming or indeterminate-time flow control
//      methods
// The idea with interrupt service routines is to try, when possible, to minimize the possibility of ISR nesting,
// and lost execution time in the main thread. Interrupts can have their priority superseded, but they cannot
// execute at the same time (e.g. with time-sharing) meaning they're not really a concurrent programming
// technique. 
//
// REENTRANCY
// Is it safe for a function or program section to be called by both the main excecution and an interrupt? Is it
// safe for it to call the same function in diferent threads at the same time? If the answer is yes, that program
// segment is called -reentrant-. 
//
// Functions that are not reentrant can typically be broken up into critical sections (where only one thread can
// access that section at a time) and reentrant sections (which are safe). If a function is running in a critical
// section in one thread, and another thread tries to call the function, an error occurs. 
//
// We already covered how to deal with reentrancy issues when talking about sharing memory, mutexs, and
// semaphores. Thread scheduling can also work to solve this issue. For concreteness, we can categorize critical
// sections as follows:
//  1. Read-modify-write sequences (reading data, modifying it, and writing it back)
//  2. Write-read sequence (writing data, then later reading it)
//  3. Nonatomic multistep write (writing data over multiple instructions)
// Clearly, atomicity is key to avoiding critical sections and ensuring reentrancy. 

// SEMAPHORES
// The different types of semaphores:
//  Spin-lock semaphore
//  Blocking semaphore
