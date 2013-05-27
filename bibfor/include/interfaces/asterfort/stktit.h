        interface
          subroutine stktit(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nlt,tit,&
     &irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: nlt
            character(len=24) :: tit
            integer :: irteti
          end subroutine stktit
        end interface
