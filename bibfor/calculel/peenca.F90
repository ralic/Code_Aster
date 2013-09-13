subroutine peenca(champ, long, vr, nbmail, nummai)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/scalai.h"
#include "asterfort/utmess.h"
    character(len=*) :: champ
    integer :: long, nbmail, nummai(*)
    real(kind=8) :: vr(long)
!     ------------------------------------------------------------------
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
!     FAIRE DES OPERATIONS SUR UN CHAM_ELEM DE TYPE ENERGIE
!            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
!     ------------------------------------------------------------------
! IN  : CHAMP  : NOM DU CHAM_ELEM
! IN  : LONG   : LONGUEUR DU VECTEUR VR
! OUT : VR     : VECTEUR CONTENANT LES RESULATTS GLOBAUX
! IN  : NBMAIL : = 0 , CALCUL SUR TOUT LE CHAM_ELEM
!                SINON CALCUL SUR UN NOMBRE DE MAILLES
! IN  : NUMMAI : NUMEROS DES MAILLES
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: longt, mode
    real(kind=8) :: rzero, ztot
    character(len=4) :: docu
    character(len=8) :: scal
    character(len=19) :: champ2, ligrel
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, icoef, idecgr, iel, im, inum
    integer :: j, jceld, jligr, k, lcelk, lvale, nbgr
    integer :: nel
!-----------------------------------------------------------------------
    call jemarq()
    champ2 = champ
    rzero = 0.0d0
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ2, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ2, 'NBSPT_1', 'STOP', ibid)
!
    call jelira(champ2//'.CELD', 'DOCU', cval=docu)
    if (docu .ne. 'CHML') then
        call utmess('F', 'CALCULEL3_52')
    endif
    call jeveuo(champ2//'.CELK', 'L', lcelk)
    ligrel = zk24(lcelk)(1:19)
!
    call jeveuo(champ2//'.CELD', 'L', jceld)
!
!     --- TYPE DE LA GRANDEUR ---
    scal= scalai(zi(jceld))
!
    nbgr = nbgrel(ligrel)
!
!     -- ON MET A ZERO LE VECTEUR "VSCAL":
    if (scal(1:1) .eq. 'R') then
        do 12 i = 1, long
            vr(i) = rzero
12      continue
    else
        call utmess('F', 'CALCULEL3_74', sk=scal)
    endif
!
    call jeveuo(champ2//'.CELV', 'L', lvale)
    if (nbmail .le. 0) then
        do 30 j = 1, nbgr
            mode=zi(jceld-1+zi(jceld-1+4+j) +2)
            if (mode .eq. 0) goto 30
            longt = digdel(mode)
            icoef=max(1,zi(jceld-1+4))
            longt = longt * icoef
            nel = nbelem(ligrel,j)
            idecgr=zi(jceld-1+zi(jceld-1+4+j)+8)
            do 32 k = 1, nel
!
!              --- TOTALE ---
                i = 1
                ztot = zr(lvale-1+idecgr+(k-1)*longt+i-1)
                vr(1) = vr(1)+ ztot
32          continue
30      continue
        vr(2) = 100.0d0
    else
        ztot = rzero
        do 34 j = 1, nbgr
            mode=zi(jceld-1+zi(jceld-1+4+j) +2)
            if (mode .eq. 0) goto 34
            longt = digdel(mode)
            icoef=max(1,zi(jceld-1+4))
            longt = longt * icoef
            nel = nbelem(ligrel,j)
            idecgr=zi(jceld-1+zi(jceld-1+4+j)+8)
            do 36 k = 1, nel
                ztot = ztot + zr(lvale-1+idecgr+(k-1)*longt)
36          continue
34      continue
        call jeveuo(ligrel//'.LIEL', 'L', jligr)
        do 40 im = 1, nbmail
            inum = 0
            do 42 j = 1, nbgr
                mode=zi(jceld-1+zi(jceld-1+4+j) +2)
                nel = nbelem(ligrel,j)
!
                if (mode .eq. 0) then
                    inum = inum + nel + 1
                    goto 42
                endif
                longt = digdel(mode)
                icoef=max(1,zi(jceld-1+4))
                longt = longt * icoef
!
                idecgr=zi(jceld-1+zi(jceld-1+4+j)+8)
                do 44 k = 1, nel
                    iel = zi(jligr+inum+k-1)
                    if (iel .ne. nummai(im)) goto 44
!
!                 --- TOTALE ---
                    i = 1
                    vr(1) = vr(1)+ zr(lvale-1+idecgr+(k-1)*longt+i-1)
                    goto 40
44              continue
                inum = inum + nel + 1
42          continue
40      continue
        if (( vr(1).lt.r8prem() ) .and. ( ztot.lt.r8prem() )) then
            vr(2) = 0.0d0
        else
            vr(2) = 100.0d0 * vr(1) / ztot
        endif
    endif
!
    call jedema()
end subroutine
