        interface
          subroutine xenrch(nomo,noma,cnslt,cnsln,cnslj,cnsen,cnsenr,&
     &ndim,fiss,goinop,lismae,lisnoe)
            character(len=8) :: nomo
            character(len=8) :: noma
            character(len=19) :: cnslt
            character(len=19) :: cnsln
            character(len=19) :: cnslj
            character(len=19) :: cnsen
            character(len=19) :: cnsenr
            integer :: ndim
            character(len=8) :: fiss
            logical :: goinop
            character(len=24) :: lismae
            character(len=24) :: lisnoe
          end subroutine xenrch
        end interface
