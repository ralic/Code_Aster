        interface
          subroutine asexc1(motfac,nbocc,nbmode,parmod,amort,corfre,&
     &ndir,valspe,asyspe)
            integer :: nbmode
            character(*) :: motfac
            integer :: nbocc
            real(kind=8) :: parmod(nbmode,*)
            real(kind=8) :: amort(*)
            logical :: corfre
            integer :: ndir(*)
            real(kind=8) :: valspe(3,*)
            real(kind=8) :: asyspe(*)
          end subroutine asexc1
        end interface
