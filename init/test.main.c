# 1 "main.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "main.c"
# 28 "main.c"
inline _syscall0(int,fork)
inline _syscall0(int,pause)
inline _syscall1(int,setup,void *,BIOS)
inline _syscall0(int,sync)
# 49 "main.c"
static char printbuf[1024];

extern char *strcpy();
extern int vsprintf();
extern void init(void);
extern void blk_dev_init(void);
extern void chr_dev_init(void);
extern void hd_init(void);
extern void floppy_init(void);
extern void mem_init(long start, long end);
extern long rd_init(long mem_start, int length);
extern long kernel_mktime(struct tm * tm);

static int sprintf(char * str, const char *fmt, ...)
{
 va_list args;
 int i;

 va_start(args, fmt);
 i = vsprintf(str, fmt, args);
 va_end(args);
 return i;
}
# 97 "main.c"
static void time_init(void)
{
 struct tm time;

 do {
  time.tm_sec = ({ outb_p(0x80|0,0x70); inb_p(0x71); });
  time.tm_min = ({ outb_p(0x80|2,0x70); inb_p(0x71); });
  time.tm_hour = ({ outb_p(0x80|4,0x70); inb_p(0x71); });
  time.tm_mday = ({ outb_p(0x80|7,0x70); inb_p(0x71); });
  time.tm_mon = ({ outb_p(0x80|8,0x70); inb_p(0x71); });
  time.tm_year = ({ outb_p(0x80|9,0x70); inb_p(0x71); });
 } while (time.tm_sec != ({ outb_p(0x80|0,0x70); inb_p(0x71); }));
 ((time.tm_sec)=((time.tm_sec)&15) + ((time.tm_sec)>>4)*10);
 ((time.tm_min)=((time.tm_min)&15) + ((time.tm_min)>>4)*10);
 ((time.tm_hour)=((time.tm_hour)&15) + ((time.tm_hour)>>4)*10);
 ((time.tm_mday)=((time.tm_mday)&15) + ((time.tm_mday)>>4)*10);
 ((time.tm_mon)=((time.tm_mon)&15) + ((time.tm_mon)>>4)*10);
 ((time.tm_year)=((time.tm_year)&15) + ((time.tm_year)>>4)*10);
 time.tm_mon--;
 startup_time = kernel_mktime(&time);
}

static long memory_end = 0;
static long buffer_memory_end = 0;
static long main_memory_start = 0;
static char term[32];

static char * argv_rc[] = { "/bin/sh", NULL };
static char * envp_rc[] = { "HOME=/", NULL ,NULL };

static char * argv[] = { "-/bin/sh",NULL };
static char * envp[] = { "HOME=/usr/root", NULL, NULL };

struct drive_info { char dummy[32]; } drive_info;

void main(void)
{




  ROOT_DEV = (*(unsigned short *)0x901FC);
  SWAP_DEV = (*(unsigned short *)0x901FA);
 sprintf(term, "TERM=con%dx%d", (((*(unsigned short *)0x9000e) & 0xff00) >> 8), ((*(unsigned short *)0x9000e) & 0xff));
 envp[1] = term;
 envp_rc[1] = term;
  drive_info = (*(struct drive_info *)0x90080);
 memory_end = (1<<20) + ((*(unsigned short *)0x90002)<<10);
 memory_end &= 0xfffff000;
 if (memory_end > 16*1024*1024)
  memory_end = 16*1024*1024;
 if (memory_end > 12*1024*1024)
  buffer_memory_end = 4*1024*1024;
 else if (memory_end > 6*1024*1024)
  buffer_memory_end = 2*1024*1024;
 else
  buffer_memory_end = 1*1024*1024;
 main_memory_start = buffer_memory_end;



 mem_init(main_memory_start,memory_end);
 trap_init();
 blk_dev_init();
 chr_dev_init();
 tty_init();
 time_init();
 sched_init();
 buffer_init(buffer_memory_end);
 hd_init();
 floppy_init();
 sti();
 move_to_user_mode();
 if (!fork()) {
  init();
 }







 for(;;)
  __asm__("int $0x80"::"a" (__NR_pause):);
}


int printf(const char *fmt, ...)
{
 va_list args;
 int i;

 va_start(args, fmt);
 write(1,printbuf,i=vsprintf(printbuf, fmt, args));
 va_end(args);
 return i;
}

void init(void)
{
 int pid,i;

 setup((void *) &drive_info);
 (void) open("/dev/tty1",O_RDWR,0);
 (void) dup(0);
 (void) dup(0);
 printf("%d buffers = %d bytes buffer space\n\r",NR_BUFFERS,
  NR_BUFFERS*BLOCK_SIZE);
 printf("Free mem: %d bytes\n\r",memory_end-main_memory_start);
 if (!(pid=fork())) {
  close(0);
  if (open("/etc/rc",O_RDONLY,0))
   _exit(1);
  execve("/bin/sh",argv_rc,envp_rc);
  _exit(2);
 }
 if (pid>0)
  while (pid != wait(&i))
                ;
 while (1) {
  if ((pid=fork())<0) {
   printf("Fork failed in init\r\n");
   continue;
  }
  if (!pid) {
   close(0);close(1);close(2);
   setsid();
   (void) open("/dev/tty1",O_RDWR,0);
   (void) dup(0);
   (void) dup(0);
   _exit(execve("/bin/sh",argv,envp));
  }
  while (1)
   if (pid == wait(&i))
    break;
  printf("\n\rchild %d died with code %04x\n\r",pid,i);
  sync();
 }
 _exit(0);
}
