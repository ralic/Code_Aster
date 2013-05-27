        interface
          subroutine impact(nmtab,nbpt,fn,vn,wk3,offset,t,elapse,&
     &nbchoc,fnmaxa,fnmmoy,fnmety,npari,lpari,valek)
            character(*) :: nmtab
            integer :: nbpt
            real(kind=8) :: fn(*)
            real(kind=8) :: vn(*)
            real(kind=8) :: wk3(*)
            real(kind=8) :: offset
            real(kind=8) :: t(*)
            real(kind=8) :: elapse
            integer :: nbchoc
            real(kind=8) :: fnmaxa
            real(kind=8) :: fnmmoy
            real(kind=8) :: fnmety
            integer :: npari
            character(len=16) :: lpari(*)
            character(len=16) :: valek(*)
          end subroutine impact
        end interface
