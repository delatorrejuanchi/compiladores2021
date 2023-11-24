#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_function5 (void** fd4_function5_args, void** fd4_m2) {
  return (void *)(({
    void** fd4_n0 = (fd4_function5_args)[1];
    ({
      void** fd4_rec_mult1_args = (fd4_function5_args)[2];
      fd4_n0
      ? (void *)(uint64_t) fd4_m2 + (uint64_t) ({
          void** fd4_clos_3 = ({
            void** fd4_clos_4 = fd4_rec_mult1_args;
            ((void* (*) (void*, void*)) (fd4_clos_4)[0])( (void *)fd4_rec_mult1_args
            , (void *)({
              fd4_sub((uint64_t) fd4_n0, (uint64_t) 1);
            }) );
          });
          ((void* (*) (void*, void*)) (fd4_clos_3)[0])( (void *)({
            void** fd4_clos_4 = fd4_rec_mult1_args;
            ((void* (*) (void*, void*)) (fd4_clos_4)[0])( (void *)fd4_rec_mult1_args
            , (void *)({
              fd4_sub((uint64_t) fd4_n0, (uint64_t) 1);
            }) );
          })
          , (void *)fd4_m2 );
        })
      : (void *)0;
    });
  }));
}
void* fd4_rec_mult1 (void** fd4_rec_mult1_args, void** fd4_n0) {
  return (void *)(fd4_mkclosure(fd4_function5, 2, fd4_n0, fd4_rec_mult1_args));
}
void* fd4_mult;
void* fd4_res;
uint64_t* fd4main() {
  fd4_mult = (void *)(fd4_mkclosure(fd4_rec_mult1, 0));
  fd4_res = (void *)(({
    void** fd4_clos_6 = ({
      void** fd4_clos_7 = fd4_mult;
      ((void* (*) (void*, void*)) (fd4_clos_7)[0])((void *)fd4_mult, (void *)3);
    });
    ((void* (*) (void*, void*)) (fd4_clos_6)[0])( (void *)({
      void** fd4_clos_7 = fd4_mult;
      ((void* (*) (void*, void*)) (fd4_clos_7)[0])((void *)fd4_mult, (void *)3);
    })
    , (void *)4 );
  }));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}