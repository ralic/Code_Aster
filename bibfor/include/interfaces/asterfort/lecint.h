        interface
          subroutine lecint(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nbg,ier,&
     &irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: nbg
            integer :: ier
            integer :: irteti
          end subroutine lecint
        end interface
