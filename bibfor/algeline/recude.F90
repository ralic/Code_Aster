subroutine recude(caelem, phie, ep)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  RECUPERATION DU DIAMETRE EXTERIEUR ET INTERIEUR D'UNE STRUCTURE
!  TUBULAIRE A PARTIR DES DONNEES FOURNIES PAR UN CONCEPT
!  DE TYPE CARA_ELEM
!  APPELANT : SPECT1 OU FLUST1, FLUST2, MDITMI VIA MDCONF
!-----------------------------------------------------------------------
!  IN : CAELEM : NOM DU CONCEPT DE TYPE CARA_ELEM
!  OUT: PHIE   : DIAMETRE EXTERIEUR DU TUBE
!  OUT: EP   :   EPAISSEUR DU TUBE
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/recugd.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: caelem
    real(kind=8) :: phie, ep
!
    character(len=8) :: nomcmp(4)
    character(len=19) :: carte
    character(len=24) :: carad
!-----------------------------------------------------------------------
    integer :: ia, iassef, iassmx, icard, ivalre, lr1, nbgd
!
    real(kind=8) :: phie2
!-----------------------------------------------------------------------
    data nomcmp  /'R1      ','EP1     ','R2      ','EP2     '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    carte = caelem(1:8)//'.CARGEOPO'
!
    carad = caelem(1:8)//'.CARGEOPO  .DESC'
!
    call jeveuo(carad, 'L', icard)
    iassmx = zi(icard+1)
    iassef = zi(icard+2)
!
    nbgd = 4
    call wkvect('&&RECUDE.TEMP.VRES', 'V V R', nbgd*iassef, ivalre)
    call recugd(carte, nomcmp, zr(ivalre), nbgd, iassef,&
                iassmx)
!
    phie2 = 0.d0
    do 10 ia = 1, iassef
        lr1 = ivalre + 4* (ia-1)
        if (zr(lr1) .ne. zr(lr1+2)) then
            call u2mess('F', 'ALGELINE3_31')
        endif
!    PAR HYPOTHESE, LA VALEUR EST NULLE S'IL NE S'AGIT
!    PAS D'UN SEGMENT
        if (zr(lr1) .eq. 0.d0) goto 10
!
        if (ia .ne. 1 .and. phie2 .ne. 0.d0) then
            if (zr(lr1) .ne. phie2) then
                call u2mess('F', 'ALGELINE3_31')
            endif
        endif
!
        phie2 = zr(lr1)
        ep = zr(lr1+1)
10  end do
!
    phie=2.d0*phie2
    if (phie .eq. 0.d0) then
        call u2mess('F', 'ALGELINE3_32')
    endif
!
    call jedetr('&&RECUDE.TEMP.VRES')
    call jedema()
!
end subroutine
