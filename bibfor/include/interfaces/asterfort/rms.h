        interface
          subroutine rms(imatr,vect1,long1,vect2,long2,nbpts,nfcod,df,&
     &nfonc)
            integer :: long2
            integer :: long1
            integer :: imatr
            real(kind=8) :: vect1(long1)
            real(kind=8) :: vect2(long2)
            integer :: nbpts
            integer :: nfcod
            real(kind=8) :: df
            integer :: nfonc
          end subroutine rms
        end interface
