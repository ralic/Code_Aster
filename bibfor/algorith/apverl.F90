subroutine apverl(sdappa)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=
    implicit     none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/apatta.h"
#include "asterfort/apninv.h"
#include "asterfort/apnndm.h"
#include "asterfort/apnomk.h"
#include "asterfort/apnumm.h"
#include "asterfort/apnumn.h"
#include "asterfort/appari.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmnorm.h"
#include "blas/ddot.h"
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - NORMALES
!
! VERIFICATION DISCRETISATION NORMALES
!
! ----------------------------------------------------------------------
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    character(len=24) :: rnomsd
    character(len=8) :: noma
    character(len=24) :: defico
    character(len=8) :: nomnom, nommam, oldnom
    integer :: nbzone, ndimg
    integer :: izone
    integer :: jdecnm, nbnom
    integer :: inom, ima, inocou, inomai
    integer :: jdec, jdeciv
    integer :: posmam, nummam, posnom, numnom
    integer :: nmanom, nnosdm
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    real(kind=8) :: taund1(3), taund2(3), normnd(3)
    real(kind=8) :: noor1, noor2
    real(kind=8) :: angmax
    character(len=24) :: aptgel, aptgno
    integer :: jtgeln, jptgno
    character(len=24) :: apverk, apvera
    integer :: jlistn, jlista
    real(kind=8) :: prosca, angle, oldang, val
    integer :: inoeu
    character(len=8) :: k8bid
    logical :: apcald
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM SD MAILLAGE
!
    call apnomk(sdappa, 'NOMA', rnomsd)
    noma = rnomsd(1:8)
    call apnomk(sdappa, 'DEFICO', defico)
!
! --- SD VERIFICATION FACETTISATION
!
    apverk = sdappa(1:19)//'.VERK'
    apvera = sdappa(1:19)//'.VERA'
    call jeveuo(apverk, 'E', jlistn)
    call jeveuo(apvera, 'E', jlista)
    call jelira(apverk, 'LONUTI', inoeu, k8bid)
    if (inoeu .ne. 0) goto 999
    angmax = 5.d0
!
! --- ACCES SD
!
    aptgel = sdappa(1:19)//'.TGEL'
    aptgno = sdappa(1:19)//'.TGNO'
    call jeveuo(aptgno, 'L', jptgno)
!
! --- INITIALISATIONS
!
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
!
! --- BOUCLE SUR LES ZONES
!
    inoeu = 0
    do 10 izone = 1, nbzone
        call apzoni(sdappa, izone, 'NBNOM', nbnom)
        call apzoni(sdappa, izone, 'JDECNM', jdecnm)
        call apzonl(sdappa, izone, 'CALC_NORM_MAIT', apcald)
        if (.not.apcald) goto 27
!
! ----- BOUCLE SUR LES NOEUDS
!
        do 20 inom = 1, nbnom
!
            posnom = inom+jdecnm
            call apnumn(sdappa, defico, posnom, numnom)
            call jenuno(jexnum(noma//'.NOMNOE', numnom), nomnom)
!
! ------- TANGENTES SUR CE NOEUD
!
            taund1(1) = zr(jptgno+6*(posnom-1)+1-1)
            taund1(2) = zr(jptgno+6*(posnom-1)+2-1)
            taund1(3) = zr(jptgno+6*(posnom-1)+3-1)
            taund2(1) = zr(jptgno+6*(posnom-1)+4-1)
            taund2(2) = zr(jptgno+6*(posnom-1)+5-1)
            taund2(3) = zr(jptgno+6*(posnom-1)+6-1)
!
! ------- CALCUL DE LA NORMALE _INTERIEURE_
!
            call mmnorm(ndimg, taund1, taund2, normnd, noor2)
!
! ------- NOMBRE DE MAILLES ATTACHEES AU NOEUD
!
            call apninv(sdappa, defico, posnom, 'NMANOM', nmanom)
!
! ------- DECALAGE POUR CONNECTIVITE INVERSE
!
            call apninv(sdappa, defico, posnom, 'JDECIV', jdeciv)
!
! ------- BOUCLE SUR LES MAILLES ATTACHEES
!
            do 30 ima = 1, nmanom
                call apatta(sdappa, defico, jdeciv, ima, posmam)
                call apnumm(sdappa, defico, posmam, nummam)
                call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
                call apnndm(sdappa, defico, posmam, nnosdm)
                call jeveuo(jexnum(noma//'.CONNEX', nummam), 'L', jdec)
                call jeveuo(jexnum(aptgel, posmam), 'L', jtgeln)
!
! --------- TRANSFERT NUMERO ABSOLU DU NOEUD -> NUMERO DANS LA CONNEC DE
! --------- LA MAILLE
!
                inocou = 0
                do 40 inomai = 1, nnosdm
                    if (zi(jdec+inomai-1) .eq. numnom) then
                        inocou = inomai
                    endif
40              continue
                call assert(inocou.ne.0)
!
! --------- RECUPERATIONS DES TANGENTES EN CE NOEUD
!
                tau1(1) = zr(jtgeln+6*(inocou-1)+1-1)
                tau1(2) = zr(jtgeln+6*(inocou-1)+2-1)
                tau1(3) = zr(jtgeln+6*(inocou-1)+3-1)
                tau2(1) = zr(jtgeln+6*(inocou-1)+4-1)
                tau2(2) = zr(jtgeln+6*(inocou-1)+5-1)
                tau2(3) = zr(jtgeln+6*(inocou-1)+6-1)
!
! --------- CALCUL DE LA NORMALE _INTERIEURE_
!
                call mmnorm(ndimg, tau1, tau2, norm, noor1)
!
! --------- CALCUL DE L'ANGLE
!
                prosca = ddot(3,norm,1,normnd,1)
                if (abs(noor1*noor2) .le. r8prem()) goto 31
                val = prosca/(noor1*noor2)
                if (val .gt. 1.d0) val = 1.d0
                if (val .lt. -1.d0) val = -1.d0
                angle = acos(val)
                angle = angle*r8rddg()
                oldang = zr(jlista+numnom-1)
                oldnom = zk8(jlistn+numnom-1)
                if (angle .gt. angmax) then
                    if (oldnom .eq. ' ') then
                        inoeu = inoeu+1
                        if (oldang .lt. angle) then
                            zk8(jlistn+numnom-1) = nomnom
                            zr(jlista+numnom-1) = angle
                        endif
                    endif
                endif
31              continue
30          continue
20      continue
27      continue
10  end do
!
    call jeecra(apverk, 'LONUTI', inoeu, k8bid)
!
999  continue
!
    call jedema()
!
end subroutine
