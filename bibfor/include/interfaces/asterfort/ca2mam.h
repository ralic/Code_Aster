        interface
          subroutine ca2mam(moint,incr,ligrmo,lchin,lpain,lpaout,num,&
     &made)
            character(*) :: moint
            character(len=3) :: incr
            character(len=24) :: ligrmo
            character(len=24) :: lchin(2)
            character(len=8) :: lpain(2)
            character(len=8) :: lpaout(1)
            character(len=14) :: num
            character(len=24) :: made
          end subroutine ca2mam
        end interface
