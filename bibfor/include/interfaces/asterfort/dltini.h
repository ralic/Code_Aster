        interface
          subroutine dltini(lcrea,nume,result,depini,vitini,accini,&
     &fexini,famini,fliini,neq,numedd,inchac,baseno)
            logical :: lcrea
            integer :: nume
            character(len=8) :: result
            real(kind=8) :: depini(*)
            real(kind=8) :: vitini(*)
            real(kind=8) :: accini(*)
            real(kind=8) :: fexini(*)
            real(kind=8) :: famini(*)
            real(kind=8) :: fliini(*)
            integer :: neq
            character(len=24) :: numedd
            integer :: inchac
            character(len=8) :: baseno
          end subroutine dltini
        end interface
