subroutine immocy(nomres, ifm)
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
!***********************************************************************
!    P. RICHARD     DATE 14/03/91
!-----------------------------------------------------------------------
!  BUT:  IMPRIMER LES RESULTATS RELATIF A UN CONCEPT MODE_CYCLIC
    implicit none
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU CONCEPT
! IFM      /I/: UNITE DU FICHIER MESSAGE
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomres, nomcou, basmod, mailla, intf
    character(len=8) :: droite, gauche, axe, valk(4)
    character(len=24) :: refe, typint, nosec, numint, diamod, freq
    character(len=24) :: cmode, desc
    integer :: ifm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iam, idiam, j, k, lldesc
    integer :: lldiam, llfre, llmoc, llnin, llnosc, llref, lltyp
    integer :: nbdax, nbddge, nbddr, nbdiam, nbmobt, nbmod, numa
    integer :: numd, numg
    real(kind=8) :: x1, xmodu, xpar
!-----------------------------------------------------------------------
    call jemarq()
!
!
!-------------------INITIALISATION DES NOMS COURANTS--------------------
!
    refe=nomres//'.CYCL_REFE'
    desc=nomres//'.CYCL_DESC'
    typint=nomres//'.CYCL_TYPE'
    nosec=nomres//'.CYCL_NBSC'
    numint=nomres//'.CYCL_NUIN'
    diamod=nomres//'.CYCL_DIAM'
    freq=nomres//'.CYCL_FREQ'
    cmode=nomres//'.CYCL_CMODE'
!
!
!
    call jeveuo(refe, 'L', llref)
    mailla=zk24(llref)(1:8)
    intf=zk24(llref+1)(1:8)
    basmod=zk24(llref+2)(1:8)
    call jeveuo(nosec, 'L', llnosc)
!
!
    valk(1)=mailla
    valk(2)=basmod
    valk(3)=intf
    call utmess('I', 'ALGELINE6_95', nk=3, valk=valk)
!
    call jeveuo(numint, 'L', llnin)
    call jeveuo(typint, 'L', lltyp)
    nomcou=zk8(lltyp)
    numd=zi(llnin)
    numg=zi(llnin+1)
    numa=zi(llnin+2)
    call jenuno(jexnum(intf//'.IDC_NOMS', numd), droite)
    call jenuno(jexnum(intf//'.IDC_NOMS', numg), gauche)
    if (numa .ne. 0) then
        call jenuno(jexnum(intf//'.IDC_NOMS', numa), axe)
    endif
!
!
    valk(1)=nomcou
    valk(2)=droite
    valk(3)=gauche
    if (numa .eq. 0) then
        call utmess('I', 'ALGELINE6_96', nk=3, valk=valk)
    else
        valk(4)=axe
        call utmess('I', 'ALGELINE6_97', nk=4, valk=valk)
    endif
!
!
    call jeveuo(desc, 'L', lldesc)
    nbmod=zi(lldesc)
    nbddr=zi(lldesc+1)
    nbdax=zi(lldesc+2)
    nbddge=nbmod+nbddr+nbdax
!
    call jelira(diamod, 'LONMAX', nbdiam)
    nbdiam=nbdiam/2
!
    call jeveuo(freq, 'L', llfre)
    call jeveuo(diamod, 'L', lldiam)
    call jeveuo(cmode, 'L', llmoc)
!
!
    iad=0
!
    do 10 i = 1, nbdiam
        idiam=zi(lldiam+i-1)
        nbmobt=zi(lldiam+nbdiam+i-1)
        call utmess('I', 'ALGELINE6_98', si=idiam)
        do 20 j = 1, nbmobt
            x1=zr(llfre+iad)
            call utmess('I', 'ALGELINE6_99', si=j, sr=x1)
            iam=(iad*nbddge)+llmoc
            xmodu=0.d0
            do 30 k = 1, nbmod
                xmodu=xmodu+(abs(zc(iam+k-1)))**2
30          continue
            xmodu=xmodu
            do 40 k = 1, nbmod
                xpar=100.d0*(abs(zc(iam+k-1))**2)/xmodu
                call utmess('I', 'ALGELINE7_1', si=k, sr=xpar)
40          continue
            write(ifm,*) ' '
            iad=iad+1
20      continue
        write(ifm,*) ' '
10  end do
!
    call jedema()
end subroutine
