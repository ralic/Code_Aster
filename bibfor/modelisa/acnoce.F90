subroutine acnoce(noma, type, liste, nb, coor,&
                  rc, xcen, tole, v1, ispv)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
!
    real(kind=8) :: coor(*), xcen(3), rc, tole, v1(3)
    character(len=8) :: noma
    character(len=24) :: liste(*)
    character(len=4) :: type
    integer :: nb, ispv
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!    VERIFICATION QUE LES MAILLES SEG2 DE LA LISTE SONT TOUTES ORIENTEES
!    DANS LE MEME SENS SUR LE CERCLE. ON TESTE PAR RAPPORT A V1
!     UTILISE PAR DEFI_ARC
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : TYPE   : 'TOUT', 'GRMA', 'LIMA'
! IN  : LISTE  : VECTEUR DE K24( NB) : LISTE DES MAILLES OU GROUPES
! IN  : NB     : DIMENSION DE LISTE
! IN  : COOR   : NOEUD EXTREMITE DE L'ENSEMBLE DES MAILLES
! IN  : RC     : RAYON DU CERCLE
! IN  : XCEN   : COORDONNES DU CENTRE DU CERCLE
! IN  : TOLE   : PRECISION DE LA VERIF
! IN  : V1     : VECTEUR DE REFERENCE
! OUT : ISPV   : SIGNE DU PRODUIT MIXTE  (XCEN-N1).(XCEN-N2),V1
! ----------------------------------------------------------------------
    integer :: vali
    character(len=24) :: mlggma, mlgnma, mlgcnx
    character(len=24) :: valk
    real(kind=8) :: x1(3), x2(3), xc1(3), xc2(3), pvec(3)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ig, im, img, ir, jdgm, jdno
    integer :: nbm, nm, nn1, nn2, nummai
    real(kind=8) :: ps, xrc1, xrc2
!-----------------------------------------------------------------------
    call jemarq()
    mlggma = noma//'.GROUPEMA'
    mlgnma = noma//'.NOMMAI'
    mlgcnx = noma//'.CONNEX'
    ispv = 0
    if (type .eq. 'TOUT') then
        call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbm, arret='C',&
                    ier=ir)
        do 52 im = 1, nbm
            call jeveuo(jexnum(mlgcnx, im), 'L', jdno)
            nn1 = zi(jdno)
            nn2 = zi(jdno+1)
            do 42 i = 1, 3
                x1(i) = coor((nn1-1)*3+i)
                x2(i) = coor((nn2-1)*3+i)
                xc1(i) = x1(i)-xcen(i)
                xc2(i) = x2(i)-xcen(i)
 42         continue
            xrc1 = padist( 3, x1, xcen )
            xrc2 = padist( 3, x2, xcen )
            if ((abs(xrc1-rc).gt.tole) .or. (abs(xrc2-rc).gt.tole)) then
                vali = im
                valk = ' '
                call utmess('E', 'MODELISA8_13', sk=valk, si=vali)
            endif
            call provec(xc1, xc2, pvec)
            ps=ddot(3,pvec,1,v1,1)
            if (ps .gt. 0.d0) then
                if (ispv .eq. 0) then
                    ispv = 1
                else if (ispv.ne.1) then
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                endif
            else if (ps.lt.0.d0) then
                if (ispv .eq. 0) then
                    ispv = -1
                else if (ispv.ne.-1) then
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                endif
            else
                vali = im
                valk = ' '
                call utmess('E', 'MODELISA8_16', sk=valk, si=vali)
            endif
 52     continue
    else if (type.eq.'GRMA') then
        do 53 ig = 1, nb
            call jeveuo(jexnom(mlggma, liste(ig)), 'L', jdgm)
            call jelira(jexnom(mlggma, liste(ig)), 'LONUTI', nm)
            do 54 im = 1, nm
                img = zi(jdgm+im-1)
                call jeveuo(jexnum(mlgcnx, img), 'L', jdno)
                nn1 = zi(jdno)
                nn2 = zi(jdno+1)
                do 44 i = 1, 3
                    x1(i) = coor((nn1-1)*3+i)
                    x2(i) = coor((nn2-1)*3+i)
                    xc1(i) = x1(i)-xcen(i)
                    xc2(i) = x2(i)-xcen(i)
 44             continue
                xrc1 = padist( 3, x1, xcen )
                xrc2 = padist( 3, x2, xcen )
                if ((abs(xrc1-rc).gt.tole) .or. (abs(xrc2-rc).gt.tole)) then
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_13', sk=valk, si=vali)
                endif
                call provec(xc1, xc2, pvec)
                ps=ddot(3,pvec,1,v1,1)
                if (ps .gt. 0.d0) then
                    if (ispv .eq. 0) then
                        ispv = 1
                    else if (ispv.ne.1) then
                        vali = im
                        valk = ' '
                        call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                    endif
                else if (ps.lt.0.d0) then
                    if (ispv .eq. 0) then
                        ispv = -1
                    else if (ispv.ne.-1) then
                        vali = im
                        valk = ' '
                        call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                    endif
                else
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_16', sk=valk, si=vali)
                endif
 54         continue
 53     continue
    else if (type.eq.'LIMA') then
        do 55 im = 1, nb
            call jenonu(jexnom(mlgnma, liste(im)), nummai)
            call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
            nn1 = zi(jdno)
            nn2 = zi(jdno+1)
            do 45 i = 1, 3
                x1(i) = coor((nn1-1)*3+i)
                x2(i) = coor((nn2-1)*3+i)
                xc1(i) = x1(i)-xcen(i)
                xc2(i) = x2(i)-xcen(i)
 45         continue
            xrc1 = padist( 3, x1, xcen )
            xrc2 = padist( 3, x2, xcen )
            if ((abs(xrc1-rc).gt.tole) .or. (abs(xrc2-rc).gt.tole)) then
                vali = im
                valk = ' '
                call utmess('E', 'MODELISA8_13', sk=valk, si=vali)
            endif
            call provec(xc1, xc2, pvec)
            ps=ddot(3,pvec,1,v1,1)
            if (ps .gt. 0.d0) then
                if (ispv .eq. 0) then
                    ispv = 1
                else if (ispv.ne.1) then
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                endif
            else if (ps.lt.0.d0) then
                if (ispv .eq. 0) then
                    ispv = -1
                else if (ispv.ne.-1) then
                    vali = im
                    valk = ' '
                    call utmess('E', 'MODELISA8_14', sk=valk, si=vali)
                endif
            else
                vali = im
                valk = ' '
                call utmess('E', 'MODELISA8_16', sk=valk, si=vali)
            endif
 55     continue
    endif
    call jedema()
end subroutine
