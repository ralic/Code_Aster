        interface
          subroutine pusure(nbpt,fn,vt1,vt2,iadh,t,pusee)
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: vt1(*)
            real(kind=8) :: vt2(*)
            integer :: iadh(*)
            real(kind=8) :: t(*)
            real(kind=8) :: pusee
          end subroutine pusure
        end interface
