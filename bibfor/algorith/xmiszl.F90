subroutine xmiszl(vecinc, defico, noma)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnsfus.h"
#include "asterfort/cnsred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: vecinc
    character(len=24) :: defico
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM ( UTILITAIRE)
!
! MISE A ZERO DES LAGRANGIENS CONTACT/FROTTEMENT DANS VECTEUR INCONNUES
!
! ----------------------------------------------------------------------
!
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! I/O VECINC : VECTEUR DES INCONNUES
!
!
!
!
    integer :: ntmae, jconx1, jconx2
    integer :: numno, nummae, nno, posmae
    integer :: ino, imae, i
    integer :: ibid, nbno, jtab, zmesx
    logical :: lcumul(4)
    real(kind=8) :: lcoefr(4)
    character(len=19) :: prno, lichs(4)
    character(len=19) :: cns1, cns1c
    character(len=19) :: cns1b, cns1d, cns1e
    integer :: jcns1b, jcns1d, jcns1e
    character(len=24) :: maescx
    integer :: jmaesx
    complex(kind=8) :: c16bid
    character(len=8) :: nommae
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    maescx = defico(1:16)// '.MAESCX'
    call jeveuo(maescx, 'L', jmaesx)
    zmesx = cfmmvd('ZMESX')
!
! --- INITIALISATIONS
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    ntmae = cfdisi(defico,'NTMAE')
    nbno = 0
!
! --- TABLEAU TEMPORAIRE POUR STOCKER NUMERO NOEUDS ESCLAVES
!
    call wkvect('&&XMISZL.NNO', 'V V I', 20*ntmae, jtab)
!
    do imae = 1, ntmae
        posmae = imae
        nummae = zi(jmaesx+zmesx*(posmae-1)+1-1)
        call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
        call jelira(jexnum(noma//'.CONNEX', nummae), 'LONMAX', nno)
        do ino = 1, nno
            numno = zi(jconx1-1+zi(jconx2+nummae-1)+ino-1)
            do i = 1, nbno
                if (numno .eq. zi(jtab+i-1)) goto 20
            end do
            nbno = nbno+1
            zi(jtab+nbno-1) = numno
 20         continue
        end do
    end do
!
! --- EXTRACTION CHAM_NO_S VECTEUR DES INCONNUES
!
    cns1 = '&&XMISZL.CNS1'
    cns1b = '&&XMISZL.CNS1B'
    cns1c = '&&XMISZL.CNS1C'
    cns1d = '&&XMISZL.CNS1D'
    cns1e = '&&XMISZL.CNS1E'
    call cnocns(vecinc, 'V', cns1)
    call dismoi('PROF_CHNO', vecinc, 'CHAM_NO', repk=prno)
!
! --- REDUCTION CHAM_NO_S SUR LAGS_C/LAG_F1/LAG_F2
!
    call cnsred(cns1, nbno, zi(jtab), 1, ['LAGS_C'],&
                'V', cns1b)
    call cnsred(cns1, nbno, zi(jtab), 1, ['LAGS_F1'],&
                'V', cns1d)
    call cnsred(cns1, nbno, zi(jtab), 1, ['LAGS_F2'],&
                'V', cns1e)
    call jeveuo(cns1b//'.CNSV', 'E', jcns1b)
    call jeveuo(cns1d//'.CNSV', 'E', jcns1d)
    call jeveuo(cns1e//'.CNSV', 'E', jcns1e)
!
! --- MISE A ZERO LAGRANGIENS
!
    do ino = 1, nbno
        zr(jcns1b-1+zi(jtab+ino-1)) = 0.d0
        zr(jcns1d-1+zi(jtab+ino-1)) = 0.d0
        zr(jcns1e-1+zi(jtab+ino-1)) = 0.d0
    end do
!
! --- FUSION CHAM_NO_S POUR CREATION CHAM_NO_S CNS1C
!
    lichs(1) = cns1
    lichs(2) = cns1b
    lichs(3) = cns1d
    lichs(4) = cns1e
    lcoefr(1) = 1.d0
    lcoefr(2) = 1.d0
    lcoefr(3) = 1.d0
    lcoefr(4) = 1.d0
    lcumul(1) = .false.
    lcumul(2) = .false.
    lcumul(3) = .false.
    lcumul(4) = .false.
    c16bid = dcmplx(0.d0, 0.d0)
    call cnsfus(4, lichs, lcumul, lcoefr, [c16bid],&
                .false., 'V', cns1c)
!
! --- CONSTRUCTION DU CHAM_NO
!
    call cnscno(cns1c, prno, 'NON', 'V', vecinc,&
                'F', ibid)
!
! --- MENAGE
!
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_NO_S', cns1b)
    call detrsd('CHAM_NO_S', cns1c)
    call detrsd('CHAM_NO_S', cns1d)
    call detrsd('CHAM_NO_S', cns1e)
    call jedetr('&&XMISZL.NNO')
!
    call jedema()
end subroutine
