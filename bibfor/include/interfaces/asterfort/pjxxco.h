        interface
          subroutine pjxxco(typcal,method,lcorre,isole,resuin,cham1,&
     &moa1,moa2,noma1,noma2,cnref,noca)
            character(len=4) :: typcal
            character(len=19) :: method
            character(len=16) :: lcorre(2)
            logical :: isole
            character(len=8) :: resuin
            character(len=19) :: cham1
            character(len=8) :: moa1
            character(len=8) :: moa2
            character(len=8) :: noma1
            character(len=8) :: noma2
            character(len=8) :: cnref
            character(len=8) :: noca
          end subroutine pjxxco
        end interface
