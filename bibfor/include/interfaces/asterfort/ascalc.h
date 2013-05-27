        interface
          subroutine ascalc(resu,masse,mome,psmo,stat,nbmode,neq,nordr&
     &,knomsy,nbopt,ndir,monoap,muapde,nbsup,nsupp,typcmo,temps,comdir,&
     &typcdi,tronc,amort,spectr,asspec,nomsup,reasup,depsup,tcosup,&
     &corfre,f1gup,f2gup)
            integer :: nbsup
            character(*) :: resu
            character(*) :: masse
            character(*) :: mome
            character(*) :: psmo
            character(*) :: stat
            integer :: nbmode
            integer :: neq
            integer :: nordr(*)
            character(*) :: knomsy(*)
            integer :: nbopt
            integer :: ndir(*)
            logical :: monoap
            logical :: muapde
            integer :: nsupp(*)
            character(*) :: typcmo
            real(kind=8) :: temps
            logical :: comdir
            character(*) :: typcdi
            logical :: tronc
            real(kind=8) :: amort(*)
            real(kind=8) :: spectr(*)
            real(kind=8) :: asspec(*)
            character(*) :: nomsup(*)
            real(kind=8) :: reasup(*)
            real(kind=8) :: depsup(*)
            integer :: tcosup(*)
            logical :: corfre
            real(kind=8) :: f1gup
            real(kind=8) :: f2gup
          end subroutine ascalc
        end interface
