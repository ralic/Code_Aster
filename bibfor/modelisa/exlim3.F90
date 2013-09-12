subroutine exlim3(motfaz, base, modelz, ligrel)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    character(len=*) :: motfaz, base, modelz, ligrel
!     -----------------------------------------------------------------
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
! ======================================================================
! BUT  :  SCRUTER LES MOTS CLE TOUT/GROUP_MA/MAILLE POUR CREER
!         UN LIGREL "REDUIT" A PARTIR DU LIGREL DU MODELE MODELZ
!
! LA DIFFERENCE AVEC EXLIMA.F EST QUE CETTE ROUTINE SCRUTE TOUTES LES
! OCCURENCES DE MOTFAZ ET DETERMINE L'EVELOPPE DE LA LISTE DES MAILLES
!
! IN  : MODELZ : NOM DU MODELE
!
! OUT/JXOUT   : LIGREL  : LIGREL REDUIT
!     ATTENTION :
!          - LE NOM DE LIGREL EST TOUJOURS "OUT"
!          - PARFOIS ON REND LIGREL=LIGREL(MODELE) :
!             - ALORS ON NE TIENT DONC PAS COMPTE DE 'BASE'
!             - IL NE FAUT PAS LE DETRUIRE !
!          - PARFOIS ON EN CREE UN NOUVEAU SUR LA BASE 'BASE'
!             - LE NOM DU LIGREL EST OBTENU PAR GNOMSD
!     -----------------------------------------------------------------
!
    integer :: ib, n1, nbma, nocc, nbmat, iocc, jnuma, jlima1
    integer :: k, numa, jlima
    character(len=8) :: modele, noma, k8bid
    character(len=16) :: motfac, motcle(2), typmcl(2)
    character(len=19) :: ligrmo
    character(len=24) :: noojb
    integer :: iarg
!     -----------------------------------------------------------------
!
    motfac=motfaz
    ASSERT(motfac.ne.' ')
    call getfac(motfac, nocc)
    ASSERT(nocc.gt.0)
!
!
    modele=modelz
    ASSERT(modele.ne.' ')
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ib,&
                ligrmo, ib)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ib,&
                noma, ib)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmat,&
                k8bid, ib)
    ASSERT(nbmat.gt.0)
!
!
!
!
!     --  SI ON DOIT TOUT PRENDRE , LIGREL = LIGRMO
!     ------------------------------------------------------
    if (getexm(motfac,'TOUT') .eq. 1) then
        do 10,iocc=1,nocc
        call getvtx(motfac, 'TOUT', iocc=iocc, scal=k8bid, nbret=n1)
        if (n1 .eq. 1) goto 60
10      continue
    endif
!
!
!
!     -- ON STOCKE DANS .NUMA, LES NUMEROS DES MAILLES DES DIFFERENTES
!        OCCURRENCES :
!     ----------------------------------------------------------------
    call wkvect('&&EXLIM3.NUMA', 'V V I', nbmat, jnuma)
!
    motcle(1)='GROUP_MA'
    motcle(2)='MAILLE'
    typmcl(1)='GROUP_MA'
    typmcl(2)='MAILLE'
!
    do 30,iocc=1,nocc
    call reliem(modele, noma, 'NU_MAILLE', motfac, iocc,&
                2, motcle(1), typmcl(1), '&&EXLIM3.LIMA1', nbma)
    ASSERT(nbma.gt.0)
    call jeveuo('&&EXLIM3.LIMA1', 'L', jlima1)
    do 20,k=1,nbma
    numa=zi(jlima1-1+k)
    zi(jnuma-1+numa)=1
20  continue
    call jedetr('&&EXLIM3.LIMA1')
    30 end do
!
!
!     -- ON FABRIQUE LA LISTE DES NUMEROS DE MAILLES POUR EXLIM1 :
!     ------------------------------------------------------------
    nbma=0
    do 40,k=1,nbmat
    if (zi(jnuma-1+k) .eq. 1) nbma=nbma+1
    40 end do
    call wkvect('&&EXLIM3.LIMA', 'V V I', nbma, jlima)
    nbma=0
    do 50,k=1,nbmat
    if (zi(jnuma-1+k) .eq. 1) then
        nbma=nbma+1
        zi(jlima-1+nbma)=k
    endif
    50 end do
    call jedetr('&&EXLIM3.NUMA')
!
!
!
!
! --- CREATION DU LIGREL
!     ---------------------------------
    noojb='12345678.LIGR000000.LIEL'
    call gnomsd(' ', noojb, 14, 19)
    ligrel=noojb(1:19)
    call exlim1(zi(jlima), nbma, modele, base, ligrel)
    call jedetr('&&EXLIM3.LIMA')
    goto 70
!
!
60  continue
    ligrel=ligrmo
!
70  continue
!
!
end subroutine
