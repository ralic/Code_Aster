        interface
          subroutine mlncmj(nb,n,p,frontl,frontu,frnl,frnu,adper,t1,t2&
     &,cl,cu)
            integer :: p
            integer :: nb
            integer :: n
            complex(kind=8) :: frontl(*)
            complex(kind=8) :: frontu(*)
            complex(kind=8) :: frnl(*)
            complex(kind=8) :: frnu(*)
            integer :: adper(*)
            complex(kind=8) :: t1(p,nb,*)
            complex(kind=8) :: t2(p,nb,*)
            complex(kind=8) :: cl(nb,nb,*)
            complex(kind=8) :: cu(nb,nb,*)
          end subroutine mlncmj
        end interface
