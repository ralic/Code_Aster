        interface
          subroutine asexc2(motfac,nbocc,nbmode,parmod,amort,corfre,&
     &noma,ndir,nomsup,nomspe,dirspe,echspe,nature,nbsupm,nsupp,knoeu,&
     &kvspe,kaspe)
            integer :: nbmode
            character(*) :: motfac
            integer :: nbocc
            real(kind=8) :: parmod(nbmode,*)
            real(kind=8) :: amort(*)
            logical :: corfre
            character(len=8) :: noma
            integer :: ndir(*)
            character(len=8) :: nomsup(3,*)
            character(len=8) :: nomspe(3,*)
            real(kind=8) :: dirspe(3,*)
            real(kind=8) :: echspe(3,*)
            integer :: nature(3,*)
            integer :: nbsupm
            integer :: nsupp(*)
            character(*) :: knoeu
            character(*) :: kvspe
            character(*) :: kaspe
          end subroutine asexc2
        end interface
