subroutine calpim(graexc, excmod, napexc, nbmode, tymmec,&
                  mtrmas, numer, nbddl, noexit, cpexit,&
                  nvasex, vecass)
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/posddl.h"
#include "asterfort/wkvect.h"
    integer :: napexc, nbmode, nbddl, nvasex
    character(len=4) :: excmod
    character(len=8) :: mtrmas, numer, tymmec, vecass(*), noexit(*), cpexit(*)
    character(len=16) :: graexc
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT:CALCUL DES COEFFICIENTS DE PARTICIPATION P:I,M  POUR
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
!-----------------------------------------------------------------------
! IN  : GRAEXC : GRANDEUR EXCITATION
! IN  : EXCMOD : L'EXCITATION EST DE TYPE MODALE
! IN  : NAPEXC : NOMBRE D APPUIS (NOEUDS OU VECTEURS ASSEMBLES
! IN  : NBMODE : NOMBRE DE MODES DYNAMIQUES
! IN  : TYMMEC : TYPE R OU C DES VALEURS DU MODE MECA
! IN  : MTRMAS : MATRICE DE MASSE
! IN  : NUMER  : CONCEPT NUMEROTATION
! IN  : NBDDL  : NOMBRE DE DDL
! IN  : NOEXIT : NOMS DES NOEUDS APPUIS
! IN  : CPEXIT : NOMS DES DDLS  APPUIS
! IN  : NVASEX : NOMBRE DE VECTEURS ASSEMBLES
! IN  : VECASS : NOMS DES VECTEURS ASSEM  APPUIS
!-----------------------------------------------------------------------
!
    integer :: iadpim, itrav1, iad,   iret,  i, ibid
    integer :: idlre1,  i4, i3, i2, i1
    real(kind=8) :: valfi
    character(len=8) :: veass1
    integer, pointer :: listadrmodsta(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: massegene(:) => null()
    integer, pointer :: listadrmode(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call wkvect('&&OP0131.PIM', 'V V R8', napexc*nbmode, iadpim)
    call wkvect('&&OP0131.TRAV1', 'V V R8', nbddl, itrav1)
!
    call jeveuo('&&OP0131.MASSEGENE', 'E', vr=massegene)
    call jeveuo('&&OP0131.LISTADRMODE', 'E', vi=listadrmode)
    call jeexin('&&OP0131.LISTADRMODSTA', iret)
    if (iret .gt. 0) call jeveuo('&&OP0131.LISTADRMODSTA', 'E', vi=listadrmodsta)
!
    call mtdscr(mtrmas)
    call jeveuo(mtrmas//'           .&INT', 'E', iad)
!
!  POUR OBTENIR JUSTE LA MATRICE DIAGONALE DES INVERSES
!  DES MASSES GENERALISEES, EXCITATION MODALE
!
    if (excmod .eq. 'OUI') then
        do 12 i = 1, nbmode
            zr(iadpim+(i-1)*nbmode+i-1) = 1.d0 / massegene(i)
12      continue
        goto 9999
    endif
!
    do 233 i1 = 1, napexc
!
        if (graexc .eq. 'DEPL_R') then
            call mrmult('ZERO', iad, zr(listadrmodsta(i1)), zr(itrav1), 1,&
                        .true._1)
        else if (nvasex .eq. 0) then
            call posddl('NUME_DDL', numer, noexit(i1), cpexit(i1), ibid,&
                        idlre1)
            zr(itrav1-1+idlre1)=1
        else
            veass1 = vecass(i1)
            call jeveuo(veass1//'           .VALE', 'L', vr=vale)
            do 237 i4 = 1, nbddl
                zr(itrav1-1+i4)=vale(i4)
237          continue
        endif
!
        do 234 i2 = 1, nbmode
            ibid = iadpim+(i1-1)*nbmode+i2-1
            zr(ibid) = 0
            if ((graexc.eq.'DEPL_R') .or. (nvasex.ne.0)) then
                do 235,i3=1,nbddl
                if (tymmec .eq. 'R') then
                    valfi = zr(listadrmode(i2)+i3-1)
                else if (tymmec .eq. 'C') then
                    valfi = dble(zc(listadrmode(i2)+i3-1))
                endif
                zr(ibid) = zr(ibid) + zr(itrav1+i3-1)*valfi
235              continue
            else
                if (tymmec .eq. 'R') then
                    valfi = zr(listadrmode(i2)+idlre1-1)
                else if (tymmec .eq. 'C') then
                    valfi = dble(zc(listadrmode(i2)+idlre1-1))
                endif
                zr(ibid) = valfi
            endif
            zr(ibid) = zr(ibid) / massegene(i2)
234      continue
233  end do
!
9999  continue
    call jedema()
end subroutine
