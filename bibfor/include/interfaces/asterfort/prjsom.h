        interface
          function prjsom(nbmat,mater,invare,invars,b,siie,type)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: invare
            real(kind=8) :: invars
            real(kind=8) :: b
            real(kind=8) :: siie
            character(len=9) :: type
            logical :: prjsom
          end function prjsom
        end interface
