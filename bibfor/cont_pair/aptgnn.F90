subroutine aptgnn(sdappa, noma , sdcont_defi, ndimg, jdecno,&
                  nbno  , itype, vector)
!
implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmmron.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtann.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=19) :: sdappa
    character(len=8) :: noma
    character(len=24) :: sdcont_defi
    integer :: ndimg, jdecno, nbno, itype
    real(kind=8) :: vector(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - TANGENTES EN CHAQUE NOEUD
!
! CALCUL SUR UNE ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  NMDIMG : DIMENSION DE L'ESPACE
! IN  JDECNO : DECALAGE POUR NUMERO DE NOEUD
! IN  NBNO   : NOMBRE DE NOEUDS DE LA ZONE
! IN  ITYPE  : NORMALE 'AUTO'(0)/'FIXE'(1)/'VECT_Y'(2)
! IN  VECTOR : POUR ITYPE = 1 OU 2
!
! ----------------------------------------------------------------------
!
    character(len=8) :: node_name, elem_name, valk(2)
    integer :: elem_indx, elem_nume, posno(1), numno(1)
    integer :: nmanom, nnosdm
    integer :: jdeciv, jdec
    integer :: ino, ima, inocou, inomai
    integer :: niverr
    real(kind=8) :: tau1(3), tau2(3), normal(3), normn
    real(kind=8) :: taund1(3), taund2(3)
    real(kind=8) :: vnorm(3), noor
    character(len=24) :: aptgel, aptgno
    integer :: jtgeln, jptgno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    aptgel = sdappa(1:19)//'.TGEL'
    aptgno = sdappa(1:19)//'.TGNO'
    call jeveuo(aptgno, 'E', jptgno)
!
! --- BOUCLE SUR LES NOEUDS
!
    do ino = 1, nbno
!
! ----- INITIALISATIONS
!
        normal(1) = 0.d0
        normal(2) = 0.d0
        normal(3) = 0.d0
        taund1(1) = 0.d0
        taund1(2) = 0.d0
        taund1(3) = 0.d0
        taund2(1) = 0.d0
        taund2(2) = 0.d0
        taund2(3) = 0.d0
!
! ----- NOEUD COURANT
!
        posno(1) = ino+jdecno
!
! ----- NUMERO ABSOLU ET NOM DU NOEUD
!
        call cfnumn(sdcont_defi, 1, posno(1), numno(1))
        call jenuno(jexnum(noma//'.NOMNOE', numno(1)), node_name)
!
! ----- Number of elements attached to node
!
        call cfnben(sdcont_defi, posno(1), 'CONINV', nmanom, jdeciv)
!
! ----- BOUCLE SUR LES MAILLES ATTACHEES
!
        do ima = 1, nmanom
!
! --------- Get elements attached to current node
!
            call cfinvm(sdcont_defi, jdeciv, ima, elem_indx)
!
! --------- Index and name of element
!
            call cfnumm(sdcont_defi, elem_indx, elem_nume)
            call jenuno(jexnum(noma//'.NOMMAI', elem_nume), elem_name)
            valk(1) = elem_name
            valk(2) = node_name
!
! ------- ACCES CONNECTIVITE DE LA MAILLE ATTACHEE
!
            call jeveuo(jexnum(noma//'.CONNEX', elem_nume), 'L', jdec)
!
! --------- Number of nodes
!
            call cfnben(sdcont_defi, elem_indx, 'CONNEX', nnosdm)
!
! ------- ACCES TANGENTES MAILLE COURANTE
!
            call jeveuo(jexnum(aptgel, elem_indx), 'L', jtgeln)
!
! ------- TRANSFERT NUMERO ABSOLU DU NOEUD -> NUMERO DANS LA CONNEC DE
! ------- LA MAILLE
!
            inocou = 0
            do inomai = 1, nnosdm
                if (zi(jdec+inomai-1) .eq. numno(1)) then
                    inocou = inomai
                endif
            end do
            ASSERT(inocou.ne.0)
!
! ------- RECUPERATIONS DES TANGENTES EN CE NOEUD
!
            tau1(1) = zr(jtgeln+6*(inocou-1)+1-1)
            tau1(2) = zr(jtgeln+6*(inocou-1)+2-1)
            tau1(3) = zr(jtgeln+6*(inocou-1)+3-1)
            tau2(1) = zr(jtgeln+6*(inocou-1)+4-1)
            tau2(2) = zr(jtgeln+6*(inocou-1)+5-1)
            tau2(3) = zr(jtgeln+6*(inocou-1)+6-1)
!
! ------- CALCUL DE LA NORMALE _INTERIEURE_
!
            call mmnorm(ndimg, tau1, tau2, vnorm, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'APPARIEMENT_15', nk=2, valk=valk)
            endif
!
! ------- NORMALE RESULTANTE
!
            normal(1) = normal(1) + vnorm(1)
            normal(2) = normal(2) + vnorm(2)
            normal(3) = normal(3) + vnorm(3)
        end do
!
! ----- MOYENNATION DE LA NORMALE SUR TOUTES LES MAILLES LIEES AU NOEUD
!
        normal(1) = normal(1) / nmanom
        normal(2) = normal(2) / nmanom
        normal(3) = normal(3) / nmanom
!
! ----- NORMALISATION NORMALE SUR TOUTES LES MAILLES LIEES AU NOEUD
!
        call normev(normal, normn)
        if (normn .le. r8prem()) then
            call utmess('F', 'APPARIEMENT_16', sk=node_name)
        endif
!
! ----- RE-CONSTRUCTION DES VECTEURS TANGENTS APRES LISSAGE
!
        call mmmron(ndimg, normal, taund1, taund2)
!
! ----- CAS PARTICULIER VECT_Y : ON REDEFINIT TAUND2
!
        if (itype .eq. 2) then
            call dcopy(3, vector, 1, taund2, 1)
            call provec(normal, taund2, taund1)
        endif
!
! ----- NORMALISATION DES TANGENTES
!
        call mmtann(ndimg, taund1, taund2, niverr)
        if (niverr .eq. 1) then
            call utmess('F', 'APPARIEMENT_17', sk=node_name)
        endif
!
! ----- STOCKAGE DES VECTEURS TANGENTS EXTERIEURS SUR LE NOEUD
!
        zr(jptgno+6*(posno(1)-1)+1-1) = taund1(1)
        zr(jptgno+6*(posno(1)-1)+2-1) = taund1(2)
        zr(jptgno+6*(posno(1)-1)+3-1) = taund1(3)
        zr(jptgno+6*(posno(1)-1)+4-1) = taund2(1)
        zr(jptgno+6*(posno(1)-1)+5-1) = taund2(2)
        zr(jptgno+6*(posno(1)-1)+6-1) = taund2(3)
    end do
!
    call jedema()
end subroutine
