subroutine vrcinp(nbvrcm, ind, instam, instap)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
use module_calcul, only : ca_iactif_, ca_jvcnom_, ca_nbcvrc_ , ca_jvcfon_, ca_jvcval_
implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/wkvect.h"
    integer :: nbvrcm, ind
    real(kind=8) :: instam, instap
! ======================================================================
!   BUT : FABRIQUER LE CHAMP DE VARIABLES DE COMMANDE CORRESPONDANT A
!         UN INSTANT DONNE POUR CALC_POINT_MAT / OP0033
!   ARGUMENTS :
!   IND      (I)   IN  : 0 => IACTIF=0 (FIN DE OP0033)
!   IND      (I)   IN  : 1 => IACTIF=1 INITIALISATIONS (DEBUT OP0033)
!   IND      (I)   IN  : 2 => INTERPOLATION EN COURS DE OP0033
!   INSTAM   (R)   IN  : VALEUR DE L'INSTANT -
!   INSTAP   (R)   IN  : VALEUR DE L'INSTANT +
! ----------------------------------------------------------------------
!
!
    integer :: nbocc, ier, n1, iocc
    character(len=19) :: tvcnom, tvcfon, tvcval
    data tvcnom/'&&OP0033.TVCNOM'/
    data tvcfon/'&&OP0033.TVCFON'/
    data tvcval/'&&OP0033.TVCVAL'/
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (ind .eq. 1) then
        ca_iactif_=2
        call getfac('AFFE_VARC', nbocc)
        ca_nbcvrc_=nbocc
        ASSERT(ca_nbcvrc_.le.nbvrcm)
!
        if (ca_nbcvrc_ .eq. 0) then
            goto 9999
        endif
!
        call wkvect(tvcnom, 'V V K8', ca_nbcvrc_, ca_jvcnom_)
        call wkvect(tvcfon, 'V V K8', ca_nbcvrc_, ca_jvcfon_)
        call wkvect(tvcval, 'V V R', 3*ca_nbcvrc_, ca_jvcval_)
        call jeveut(tvcnom, 'E', ca_jvcnom_)
        call jeveut(tvcfon, 'E', ca_jvcfon_)
        call jeveut(tvcval, 'E', ca_jvcval_)
!
        do 10 iocc = 1, nbocc
!           ON STOCKE LES NOMS PUIS LES FONCTIONS
            call getvtx('AFFE_VARC', 'NOM_VARC', iocc=iocc, scal=zk8( ca_jvcnom_-1+iocc), nbret=n1)
            call getvid('AFFE_VARC', 'VALE_FONC', iocc=iocc, scal=zk8( ca_jvcfon_-1+iocc), nbret=n1)
!           AJOUTER LES FONCTIONS DEVRIVANT LES PHASES METALLURGIQUES
            call getvr8('AFFE_VARC', 'VALE_REF', iocc=iocc, scal=zr( ca_jvcval_-1+3*(iocc-1)+3),&
                        nbret=n1)
!
10      continue
!
    else if (ind.eq.0) then
        ca_iactif_=0
!
    else if (ind.eq.2) then
        ca_iactif_=2
!        EVALUATION DES FONCTIONS
        do 20 iocc = 1, ca_nbcvrc_
            call fointe('F', zk8(ca_jvcfon_-1+iocc), 1, ['INST'], [instam],&
                        zr( ca_jvcval_-1+3*(iocc-1)+1), ier)
            call fointe('F', zk8(ca_jvcfon_-1+iocc), 1, ['INST'], [instap],&
                        zr( ca_jvcval_-1+3*(iocc-1)+2), ier)
20      continue
!
    endif
!
9999  continue
    call jedema()
end subroutine
