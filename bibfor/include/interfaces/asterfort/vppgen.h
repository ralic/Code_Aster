        interface
          subroutine vppgen(lmasse,lamor,lraide,masseg,amorg,raideg,&
     &vect,neq,nbvect,iddl)
            integer :: neq
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            real(kind=8) :: masseg(*)
            real(kind=8) :: amorg(*)
            real(kind=8) :: raideg(*)
            real(kind=8) :: vect(neq,*)
            integer :: nbvect
            integer :: iddl(*)
          end subroutine vppgen
        end interface
