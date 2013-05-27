subroutine te0452(option, nomte)
    implicit  none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/excent.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecach.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPTION : EFGE_ELNO_EXCENT
!     IN   K16   OPTION : NOM DE L'OPTION A CALCULER
!     IN   K16   NOMTE  : NOM DU TYPE_ELEMENT
!     ------------------------------------------------------------------
    integer :: itab1(7), itab2(7), iret, jin, jout, lgcata
    integer :: nbpoin, nbcmp, jcara, ibid
    logical :: lreel
    real(kind=8) :: excen
!     ------------------------------------------------------------------
!
    call assert(option.eq.'EFGE_EXCENT')
!
!     -- IL Y A 4 CAS POSSIBLES : GAUSS/NOEUD + REEL/COMPLEXE
    call tecach('ONO', 'PEFFONR', 'L', 7, itab1,&
                iret)
    if (iret .eq. 0) then
        lreel=.true.
        call tecach('OOO', 'PEFFOENR', 'E', 7, itab2,&
                    ibid)
    else
        call tecach('ONO', 'PEFFONC', 'L', 7, itab1,&
                    iret)
        if (iret .eq. 0) then
            lreel=.false.
            call tecach('OOO', 'PEFFOENC', 'E', 7, itab2,&
                        ibid)
        else
            call tecach('ONO', 'PEFFOGR', 'L', 7, itab1,&
                        iret)
            if (iret .eq. 0) then
                lreel=.true.
                call tecach('OOO', 'PEFFOEGR', 'E', 7, itab2,&
                            ibid)
            else
                lreel=.false.
                call tecach('OOO', 'PEFFOGC', 'L', 7, itab1,&
                            ibid)
                call tecach('OOO', 'PEFFOEGC', 'E', 7, itab2,&
                            ibid)
            endif
        endif
    endif
!
    jin=itab1(1)
    nbpoin=itab1(3)
    lgcata=itab1(2)
    nbcmp=lgcata/nbpoin
    call assert(lgcata.eq.nbpoin*nbcmp)
    call assert(nbcmp.eq.6.or.nbcmp.eq.8)
!
    jout=itab2(1)
    call assert(itab2(2).eq.lgcata)
!
    call jevech('PCACOQU', 'L', jcara)
    excen=zr(jcara+5-1)
!
    call excent('MOY', excen, nbpoin, nbcmp, lreel,&
                zr(jin), zr(jout), zc(jin), zc(jout))
!
!
!
end subroutine
