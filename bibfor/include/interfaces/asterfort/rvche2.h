        interface
          subroutine rvche2(chelez,nomjv,nbel,numail,orig,axez,nbnac,&
     &nnoeud)
            character(*) :: chelez
            character(*) :: nomjv
            integer :: nbel
            integer :: numail(*)
            real(kind=8) :: orig(3)
            real(kind=8) :: axez(3)
            integer :: nbnac
            integer :: nnoeud(*)
          end subroutine rvche2
        end interface
