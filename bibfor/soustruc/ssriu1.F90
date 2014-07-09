subroutine ssriu1(nomu)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nueq_chck.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomu
! ----------------------------------------------------------------------
!     BUT:
!       1) METTRE A JOUR .DESM(3,4,5,8,9,10)
!       2) METTRE LES DDLS INTERNES AVANT LES DLLS EXTERNES.
!          ON CHANGE LE VECTEUR D'INDIRECTION .NUEQ DU PROF_CHNO
!       3) ECRIRE LE "LONMAX" DE LA COLLECTION .LICA
!       4) CALCULER L'OBJET .CONX :
!          I VARIE DE 1 A NBNOET=NBNOE+NLAGE+NLAGL
!          (L'ORDRE EST CELUI DE LA NUMEROTAION DE LA MATRICE DE RIGI)
!          CONX(I,1) CONTIENT LE NUMERO DU LIGREL OU EST DEFINI LE
!                      I_EME NOEUD "EXTERNE"
!          CONX(I,2) CONTIENT LE NUMERO (DANS LE LIGREL)
!                      DU I_EME NOEUD "EXTERNE"
!          CONX(I,3) = -0 SI LE NOEUD EST PHYSIQUE
!                    = -1 SI LE NOEUD EST DE LAGRANGE (TYPE "AVANT).
!                    = -2 SI LE NOEUD EST DE LAGRANGE (TYPE "APRES").
!
!        ON SUPPOSE QU'AVANT L'APPEL A CETTE ROUTINE, L'OBJET .NUEQ
!        EST L'INDIRECTION "IDENTITE" (ZI(IANUEQ-1 +I) = I).
!
!        ON "DEPLACE" VERS L'AVANT (MODIFICATION DES ADRESSES VIA .NUEQ)
!        LES GROUPES DE DDLS PORTES PAR LES NOEUDS INTERNES.
!        ON SE SERT POUR CELA DE .DEEQ.
!
!          REGLE ADOPTEE POUR LES DDDLS/NOEUDS DE LAGRANGE:
!          - ON NE GARDE COMME DDLS INTERNES QUE :
!             - LES DDLS PHYSIQUES DES NOEUDS INTERNES.
!             - LES DDLS LAGRANGES ASSOCIES A
!               DES BLOCAGES DE NOEUDS INTERNES
!          - LES DDLS LAGRANGES ASSOCIES A DES LIAISONS SONT DONC
!            EXTERNES.
!
!       EXEMPLE:   NOEUDS INTERNES : 1,3
!                  NOEUDS EXTERNES : 2
!
!       DEEQ       NUEQ        CEUX QUI      NUEQ       "CONX"
!                  (AVANT)     REMONTENT (APRES)
!        1, 1         1           X           1           - - -
!        1, 2         2           X           2           - - -
!        0, 0         3                       7           2 1 -1
!        2,-1         4                       8           1 2 0
!        2,-2         5                       9           - - -
!        2, 1         6                      10           - - -
!        2, 2         7                      11           - - -
!        2,-1         8                      12           - - -
!        2,-2         9                      13           - - -
!        3,-1        10           X           3           - - -
!        3, 1        11           X           4           - - -
!        3, 2        12           X           5           - - -
!        3,-1        13           X           6           - - -
!        0, 0        14                      14           2 2 -2
!
!     IN: NOMU   : NOM DU MACR_ELEM_STAT
!
!     OUT: L OBJET .DESM EST MODIFIE.
!          L OBJET .NUEQ EST MODIFIE.
!          L OBJET .DEEQ EST MODIFIE.
!          L OBJET .DELG EST MODIFIE.
!
! ----------------------------------------------------------------------
!
!
    integer :: i
    character(len=8) :: nogdsi
    character(len=14) :: nume_ddl
    character(len=19) :: prof_chno
    integer :: iaconx
    integer :: iaprno,   ico, icoe, icoi
    integer :: ieqn, ili, inl, ino, iret
    integer :: itylag, n1, nbno, nbnoe, nbnoet, nddle, nddli
    integer :: nddlt, nec, nlage, nlagi, nlagl, nlili, nuddl
    integer :: nueq, nulag, nuno, nuno2, nunold
    integer, pointer :: interne(:) => null()
    integer, pointer :: work1(:) => null()
    integer, pointer :: work2(:) => null()
    integer, pointer :: vnueq(:) => null()
    integer, pointer :: delg(:) => null()
    integer, pointer :: deeq(:) => null()
    integer, pointer :: lino(:) => null()
    integer, pointer :: desm(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    nume_ddl = nomu
    prof_chno = nume_ddl(1:14)//'.NUME'
!
    call dismoi('NOM_GD', nume_ddl, 'NUME_DDL', repk=nogdsi)
    if (nogdsi .ne. 'DEPL_R') then
        call utmess('F', 'SOUSTRUC_70')
    endif
    call dismoi('NU_CMP_LAGR', 'DEPL_R', 'GRANDEUR', repi=nulag)
    call dismoi('NB_EC', nogdsi, 'GRANDEUR', repi=nec)

    call nueq_chck(prof_chno, nddlt)
    call jeveuo(prof_chno//'.DEEQ', 'E', vi=deeq)
    call jeveuo(prof_chno//'.DELG', 'E', vi=delg)
    call jeveuo(prof_chno//'.NUEQ', 'E', vi=vnueq)
    call jelira(prof_chno//'.PRNO', 'NMAXOC', nlili)
!
    call jeveuo(nomu//'.DESM', 'E', vi=desm)
    call jeveuo(nomu//'.LINO', 'E', vi=lino)
    nbnoe = desm(2)
!
!
!     -- ALLOCATION D'UN VECTEUR DE TRAVAIL QUI CONTIENDRA
!        DES "1" SUR LES DDL INTERNES.
    AS_ALLOCATE(vi=interne, size=nddlt)
!
!
!     -- BOUCLE SUR LES  DDLS, REMPLISSAGE DE .INTERNE:
!     -------------------------------------------------
    nunold = 0
    icoi = 0
    icoe = 0
    nddli = 0
    nlagl = 0
    nlagi = 0
    nlage = 0
!
    do i = 1, nddlt
        ASSERT(vnueq(i).eq.i)
!
        nuno = deeq(2* (i-1)+1)
        nuddl = deeq(2* (i-1)+2)
!
!        -- LES LAGRANGES DU MAILLAGE SONT TOUS DECLARES EXTERNES:
!           (ON LES CONSERVERA DONC A TOUS LES NIVEAUX)
        if (nuddl .eq. nulag) then
            nlage = nlage + 1
            goto 10
        endif
!
        if (nuno .ne. 0) then
            nuno2 = indiis(lino,nuno,1,nbnoe)
            if (nuno2 .eq. 0) then
                interne(i) = 1
                nddli = nddli + 1
            endif
!
!           -- ON COMPTE LES LAGRANGES INTERNES ET EXTERNES:
            if (nuddl .lt. 0) then
                if (nuno2 .eq. 0) then
                    nlagi = nlagi + 1
                else
                    nlage = nlage + 1
                endif
            endif
!
!           -- ON COMPTE LES NOEUDS INTERNES ET EXTERNES:
            if ((nuddl.gt.0) .and. (nunold.ne.nuno)) then
                nunold = nuno
                if (nuno2 .eq. 0) then
                    icoi = icoi + 1
                else
                    icoe = icoe + 1
                    ASSERT(icoe.le.nbnoe)
                endif
            endif
        else
            nlagl = nlagl + 1
        endif
 10     continue
    end do
!
    ASSERT(nbnoe.eq.icoe)
    if (icoi .eq. 0) then
        call utmess('F', 'SOUSTRUC_71')
    endif
    desm(3) = icoi
    nddle = nddlt - nddli
    desm(4) = nddle
    desm(5) = nddli
    desm(8) = nlage
    desm(9) = nlagl
    desm(10) = nlagi
!
!     -- DIMENSIONNEMENT DES OBJETS DE LA COLLECTION .LICA:
!     -----------------------------------------------------
    call jeecra(nomu//'.LICA', 'LONMAX', 2*nddlt)
!
!
!     -- MODIFICATION DE .NUEQ:
!     -------------------------
    AS_ALLOCATE(vi=work2, size=nddlt)
!    .WORK2 CONTIENT LA RECIPROQUE DU NOUVEAU .NUEQ:
    ico = 0
!     -- ON CLASSE LES DDLS INTERNES:
    do i = 1, nddlt
        if (interne(i) .eq. 1) then
            ico = ico + 1
            vnueq(i) = ico
            work2(ico) = i
        endif
    end do
!
!     -- ON CLASSE LES DDLS EXTERNES:
    do i = 1, nddlt
        if (interne(i) .eq. 0) then
            ico = ico + 1
            vnueq(i) = ico
            work2(ico) = i
        endif
    end do
!
!
!     -- CREATION DE .CONX:
!     ---------------------
    nbnoet = nlage + nlagl + nbnoe
    call wkvect(nomu//'.CONX', 'G V I', 3*nbnoet, iaconx)
    AS_ALLOCATE(vi=work1, size=2*nddlt)
    ico = 0
    nunold = 0
!
!     -- MISE A JOUR DE .CONX : NOEUDS DU MAILLAGE + TYPE_LAGRANGE :
!     ------------------------------------------------------------
!     --ON TRAVAILLE AVEC L'ANCIEN .DEEQ:
    do i = 1, nddlt
        nuno = deeq(2* (i-1)+1)
        nuddl = deeq(2* (i-1)+2)
!        -- ITYLAG EST LE TYPE DU NOEUD DE LAGRANGE (-1 OU -2)
        itylag = delg(i)
        if (nuno .ne. 0) then
            nuno2 = indiis(lino,nuno,1,nbnoe)
!
!           -- TYPE LAGRANGE DES NOEUDS SUPPLEMENTAIRES:
            if (nuddl .lt. 0) then
                if (nuno2 .ne. 0) then
                    ico = ico + 1
                    zi(iaconx-1+3* (ico-1)+3) = itylag
                    ieqn = vnueq(i)
                    ASSERT(ieqn.gt.nddli)
                    work1(ieqn) = ico
                endif
            endif
!
!           -- NOEUDS LAGRANGES DU MAILLAGE :
            if (nuddl .eq. nulag) then
                ico = ico + 1
                zi(iaconx-1+3* (ico-1)+1) = 1
                zi(iaconx-1+3* (ico-1)+2) = nuno
                zi(iaconx-1+3* (ico-1)+3) = itylag
                ieqn = vnueq(i)
                ASSERT(ieqn.gt.nddli)
                work1(ieqn) = ico
            endif
!
!           -- NOEUDS PHYSIQUES DU MAILLAGE :
            if ((nuddl.gt.0) .and. (nunold.ne.nuno)) then
                nunold = nuno
                if (nuno2 .ne. 0) then
                    ico = ico + 1
                    zi(iaconx-1+3* (ico-1)+1) = 1
                    zi(iaconx-1+3* (ico-1)+2) = nuno
                endif
            endif
        else
!
!           -- NOEUDS LAGRANGE DES LIAISONS DDL :
            ico = ico + 1
            zi(iaconx-1+3* (ico-1)+3) = itylag
            ieqn = vnueq(i)
            ASSERT(ieqn.gt.nddli)
            work1(ieqn) = ico
        endif
    end do
!
!     -- MISE A JOUR DE .CONX : NOEUDS DE LAGRANGE :
!     ----------------------------------------------
    do ili = 2, nlili
        call jeexin(jexnum(prof_chno//'.PRNO', ili), iret)
        if (iret .eq. 0) goto 60
        call jelira(jexnum(prof_chno//'.PRNO', ili), 'LONMAX', n1)
        if (n1 .eq. 0) goto 60
        call jeveuo(jexnum(prof_chno//'.PRNO', ili), 'L', iaprno)
        nbno = n1/ (nec+2)
        do ino = 1, nbno
            nueq = zi(iaprno-1+ (ino-1)* (nec+2)+1)
            if (nueq .eq. 0) goto 50
            ieqn = vnueq(nueq)
            if (ieqn .gt. nddli) then
                inl = work1(ieqn)
                zi(iaconx-1+3* (inl-1)+1) = ili
                zi(iaconx-1+3* (inl-1)+2) = ino
            endif
 50         continue
        end do
 60     continue
    end do
!
!
!     -- REMISE EN ORDRE DE .DEEQ ET .DELG POUR TENIR COMPTE
!        DE LA MODIFICATION DE .NUEQ :
!        ---------------------------------------------------
    do i = 1, nddlt
        work1(i) = delg(work2(i))
    end do
    do i = 1, nddlt
        delg(i) = work1(i)
    end do
!
    do i = 1, nddlt
        work1(2* (i-1)+1) = deeq(2* (work2(i)-1)+1)
        work1(2* (i-1)+2) = deeq(2* (work2(i)-1)+2)
    end do
    do i = 1, 2*nddlt
        deeq(i) = work1(i)
    end do
!
!
!     -- ON REMET .LINO DANS UN ORDRE COHERENT AVEC .CONX:
!        ---------------------------------------------------
    ico = 0
    do i = 1, nbnoet
!
!     -- SI C'EST UN NOEUD PHYSIQUE DU MAILLAGE :
        if ((zi(iaconx-1+3* (i-1)+1).eq.1) .and. (zi(iaconx-1+3* (i-1) +3).eq.0)) then
            ico = ico + 1
            lino(ico) = zi(iaconx-1+3* (i-1)+2)
        endif
    end do
!
! --- MENAGE
    AS_DEALLOCATE(vi=interne)
    AS_DEALLOCATE(vi=work1)
    AS_DEALLOCATE(vi=work2)
!
    call jedema()
end subroutine
