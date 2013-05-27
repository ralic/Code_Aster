        interface
          subroutine nmobsz(sdobse,nomtab,titobs,nomcha,typcha,extrch,&
     &extrcp,extrga,nomcmp,nomnoe,nommai,num,snum,instan,valr)
            character(len=19) :: sdobse
            character(len=19) :: nomtab
            character(len=80) :: titobs
            character(len=24) :: nomcha
            character(len=4) :: typcha
            character(len=8) :: extrch
            character(len=8) :: extrcp
            character(len=8) :: extrga
            character(len=8) :: nomcmp
            character(len=8) :: nomnoe
            character(len=8) :: nommai
            integer :: num
            integer :: snum
            real(kind=8) :: instan
            real(kind=8) :: valr
          end subroutine nmobsz
        end interface
