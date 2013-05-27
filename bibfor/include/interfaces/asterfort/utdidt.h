        interface
          subroutine utdidt(getset,sddisc,typque,iocc,quest,valr,vali,&
     &valk)
            character(len=1) :: getset
            character(len=19) :: sddisc
            character(len=4) :: typque
            integer :: iocc
            character(*) :: quest
            real(kind=8) :: valr
            integer :: vali
            character(*) :: valk
          end subroutine utdidt
        end interface
