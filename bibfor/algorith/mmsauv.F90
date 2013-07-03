subroutine mmsauv(resoco, izone, iptc, nummam, ksipr1,&
                  ksipr2, tau1, tau2, nummae, numnoe,&
                  ksipc1, ksipc2, wpc)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: iptc, izone
    integer :: nummam, nummae, numnoe
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: ksipc1, ksipc2
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: wpc
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUES - APPARIEMENT)
!
! SAUVEGARDE APPARIEMENT - CAS MAIT_ESCL
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  IPTC   : NUMERO DE LA LIAISON DE CONTACT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  NUMMAM : NUMERO ABSOLU MAILLE MAITRE QUI RECOIT LA PROJECTION
! IN  NUMMAE : NUMERO ABSOLU MAILLE ESCLAVE
! IN  NUMNOE : NUMERO ABSOLU DU PT INTEG DANS LES SD CONTACT SI
!              INTEG.NOEUDS
! IN  KSIPR1 : PREMIERE COORDONNEE PARAMETRIQUE PT CONTACT PROJETE
!              SUR MAILLE MAITRE
! IN  KSIPR2 : SECONDE COORDONNEE PARAMETRIQUE PT CONTACT PROJETE
!              SUR MAILLE MAITRE
! IN  TAU1   : PREMIERE TANGENTE
! IN  TAU2   : SECONDE TANGENTE
! IN  KSIPC1 : PREMIERE COORDONNEE PARAMETRIQUE PT CONTACT
! IN  KSIPC2 : SECONDE COORDONNEE PARAMETRIQUE PT CONTACT
! IN  WPC    : POIDS INTEGRATION
!
!
!
!
    integer :: ztabf
    character(len=24) :: tabfin
    integer :: jtabf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
    call assert(izone.gt.0)
!
! --- STOCKAGE DES VALEURS POUR LE CHAM_ELEM (VOIR MMCHML)
!
    zr(jtabf+ztabf*(iptc-1)+2) = nummam
    zr(jtabf+ztabf*(iptc-1)+5) = ksipr1
    zr(jtabf+ztabf*(iptc-1)+6) = ksipr2
    zr(jtabf+ztabf*(iptc-1)+7) = tau1(1)
    zr(jtabf+ztabf*(iptc-1)+8) = tau1(2)
    zr(jtabf+ztabf*(iptc-1)+9) = tau1(3)
    zr(jtabf+ztabf*(iptc-1)+10) = tau2(1)
    zr(jtabf+ztabf*(iptc-1)+11) = tau2(2)
    zr(jtabf+ztabf*(iptc-1)+12) = tau2(3)
    zr(jtabf+ztabf*(iptc-1)+13) = izone
!
! --- STOCKAGE DES VALEURS POUR LE CHAM_ELEM (VOIR MMCHML)
!
    zr(jtabf+ztabf*(iptc-1)+1) = nummae
    zr(jtabf+ztabf*(iptc-1)+3) = ksipc1
    zr(jtabf+ztabf*(iptc-1)+4) = ksipc2
    zr(jtabf+ztabf*(iptc-1)+14) = wpc
    zr(jtabf+ztabf*(iptc-1)+24) = numnoe
!
    call jedema()
end subroutine
