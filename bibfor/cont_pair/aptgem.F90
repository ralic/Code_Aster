subroutine aptgem(sdappa, noma  , newgeo, sdcont_defi, ndimg ,&
                  izone , typzon, itemax, epsmax     , jdecma,&
                  nbma)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/apcoma.h"
#include "asterfort/apcond.h"
#include "asterfort/apcpoi.h"
#include "asterfort/apcpou.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/aptypm.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmctan.h"
#include "asterfort/mmtann.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: sdcont_defi
    character(len=19) :: sdappa, newgeo
    character(len=8) :: noma
    integer :: ndimg, izone, jdecma, nbma
    character(len=4) :: typzon
    integer :: itemax
    real(kind=8) :: epsmax
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - TANGENTES EN CHAQUE NOEUD D'UNE ELEMENT
!
! CALCUL SUR UNE ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  NEWGEO : CHAMP DE GEOMETRIE ACTUALISE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  NMDIMG : DIMENSION DE L'ESPACE
! IN  IZONE  : NUMERO DE LA ZONE
! IN  TYPZON : TYPE DE LA ZONE 'MAIT' OU 'ESCL'
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
! IN  JDECMA : DECALAGE POUR NUMERO DE MAILLE
! IN  NBMA   : NOMBRE DE MAILLES DE LA ZONE
!
!
!
!
    character(len=8) :: alias, elem_name, node_name, valk(2)
    integer :: numno(9), longc
    integer :: nnosdm, niverr
    aster_logical :: lpoutr, lpoint
    integer :: jtgeln, jdec
    integer :: ino, ima, ndim
    integer :: elem_indx, elem_nume
    real(kind=8) :: tau1(3), tau2(3)
    character(len=24) :: aptgel
    real(kind=8) :: coorma(27), coorno(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SDAPPA
!
    aptgel = sdappa(1:19)//'.TGEL'
!
! --- BOUCLE SUR LES MAILLES
!
    do ima = 1, nbma
!
        elem_indx = ima+jdecma
!
! ----- Index of element
!
        call cfnumm(sdcont_defi, elem_indx, elem_nume)
!
! ----- Number of nodes
!
        call cfnben(sdcont_defi, elem_indx, 'CONNEX', nnosdm)
!
! ----- CARACTERISTIQUES DE LA MAILLE MAITRE
!
        call aptypm(sdappa, noma, elem_nume, ndim, nnosdm,&
                    alias, elem_name)
!
! ----- COORDONNNEES DE LA MAILLE MAITRE
!
        call apcoma(sdappa, noma, newgeo, elem_nume, nnosdm,&
                    coorma)
!
! ----- NUMEROS ABSOLUS DES NOEUDS DE LA MAILLE
!
        call jeveuo(jexnum(noma//'.CONNEX', elem_nume), 'L', jdec)
        do ino = 1, nnosdm
            numno(ino) = zi(jdec+ino-1)
        end do
!
! ----- LONGUEUR EFFECTIVE
!
        call jelira(jexnum(aptgel, elem_indx), 'LONMAX', longc)
        longc = longc /6
!
! ----- TYPE DE MAILLE
!
        lpoutr = (alias(1:2).eq.'SE').and.(ndimg.eq.3)
        lpoint = alias.eq.'PO1'
!
! ----- ACCES MAILLE COURANTE
!
        call jeveuo(jexnum(aptgel, elem_indx), 'E', jtgeln)
!
! ----- BOUCLE SUR LES NOEUDS DE LA MAILLE
!
        do ino = 1, nnosdm
!
! ------- COORDONNNEES ET NOM DU NOEUD
!
            call apcond(sdappa, newgeo, numno(ino), coorno)
            call jenuno(jexnum(noma//'.NOMNOE', numno(ino)), node_name)
            valk(1) = elem_name
            valk(2) = node_name
!
! ------- INITIALISATIONS
!
            tau1(1) = r8maem()
            tau1(2) = r8maem()
            tau1(3) = r8maem()
            tau2(1) = r8maem()
            tau2(2) = r8maem()
            tau2(3) = r8maem()
!
! ------- CALCUL DES TANGENTES EN CE NOEUD
!
            if (lpoint) then
!
! --------- ELEMENT POINT
!
                call apcpoi(sdappa, ndimg, izone, elem_name, typzon,&
                            tau1, tau2)
            else
!
! --------- AUTRES ELEMENTS
!
                call mmctan(elem_name, alias, nnosdm, ndim, coorma,&
                            coorno, itemax, epsmax, tau1, tau2)
!
                if (lpoutr) then
!
! --------- CAS PARTICULIER : ELEMENT POUTRE
!
                    call apcpou(sdappa, izone, elem_name, typzon, tau1,&
                                tau2)
                endif
!
            endif
!
! ------- NORMALISATION DES TANGENTES
!
            call mmtann(ndimg, tau1, tau2, niverr)
!
            if (niverr .eq. 1) then
                call utmess('F', 'APPARIEMENT_14', nk=2, valk=valk)
            endif
!
! ------- STOCKAGE DES TANGENTES
!
            if (ino .le. longc) then
! --------- CE TEST PROTEGE CONTRE LES QUAD8 QUI N'ONT QUE 4 NOEUDS !
                zr(jtgeln+6*(ino-1)+1 -1) = tau1(1)
                zr(jtgeln+6*(ino-1)+2 -1) = tau1(2)
                zr(jtgeln+6*(ino-1)+3 -1) = tau1(3)
                zr(jtgeln+6*(ino-1)+4 -1) = tau2(1)
                zr(jtgeln+6*(ino-1)+5 -1) = tau2(2)
                zr(jtgeln+6*(ino-1)+6 -1) = tau2(3)
            endif
        end do
    end do
!
    call jedema()
end subroutine
