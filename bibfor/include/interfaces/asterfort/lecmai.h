        interface
          subroutine lecmai(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nbg,fmt,dim,&
     &nbt,ier,irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: nbg
            integer :: fmt(nbm)
            integer :: dim(nbm)
            integer :: nbt(nbm)
            integer :: ier
            integer :: irteti
          end subroutine lecmai
        end interface
