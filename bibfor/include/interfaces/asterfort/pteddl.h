        interface
          subroutine pteddl(typesd,num,nbcmp,lnocmp,neq,ivec)
            integer :: neq
            character(*) :: typesd
            character(*) :: num
            integer :: nbcmp
            character(len=8) :: lnocmp(*)
            integer :: ivec(neq,*)
          end subroutine pteddl
        end interface
