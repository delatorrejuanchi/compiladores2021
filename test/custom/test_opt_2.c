#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_a;
void* fd4_b;
void* fd4_function2 (void** fd4_function2_args, void** fd4_m1) {
  return (void *)(({
    void** fd4_n0 = (fd4_function2_args)[1];
    (uint64_t) fd4_n0 + (uint64_t) fd4_m1;
  }));
}
void* fd4_function3 (void** fd4_function3_args, void** fd4_n0) {
  return (void *)(fd4_mkclosure(fd4_function2, 1, fd4_n0));
}
void* fd4_suma;
void* fd4_res;
uint64_t* fd4main() {
  fd4_a = (void *)(8);
  fd4_b = (void *)(5);
  fd4_suma = (void *)(fd4_mkclosure(fd4_function3, 0));
  fd4_res = (void *)(({
    void** fd4_clos_4 = ({
      void** fd4_clos_5 = fd4_suma;
      ((void* (*) (void*, void*)) (fd4_clos_5)[0])( (void *)fd4_suma
      , (void *)fd4_a );
    });
    ((void* (*) (void*, void*)) (fd4_clos_4)[0])( (void *)({
      void** fd4_clos_5 = fd4_suma;
      ((void* (*) (void*, void*)) (fd4_clos_5)[0])( (void *)fd4_suma
      , (void *)fd4_a );
    })
    , (void *)fd4_b );
  }));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}