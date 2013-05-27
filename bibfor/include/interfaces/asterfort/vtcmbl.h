        interface
          subroutine vtcmbl(nbcmb,typcst,const,typech,nomch,typres,&
     &chpres)
            integer :: nbcmb
            character(*) :: typcst(*)
            real(kind=8) :: const(*)
            character(*) :: typech(*)
            character(*) :: nomch(*)
            character(*) :: typres
            character(*) :: chpres
          end subroutine vtcmbl
        end interface
