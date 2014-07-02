subroutine ssvaro(l, sens, matrix, typnoe, nomacr,&
                  iadm1, iadm2)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomacr
    character(len=*) :: sens
    aster_logical :: matrix
    character(len=4) :: typnoe
    integer :: iadm1, iadm2
    real(kind=8) :: l(6, 6)
! ----------------------------------------------------------------------
!     BUT:
!         "TOURNER" LA MATRICE ELEMENTAIRE (OU LE VECTEUR ELEMENTAIRE)
!          D'UN MACR_ELEM_STAT
!          A L'AIDE DE LA MATRICE DE CHANGEMENT DE REPERE L.
!
!     PRECAUTION:
!         CETTE ROUTINE EST ECRITE POUR LA GRANDEUR "DEPL_R".
!         LES 4 SEULS CAS DE FIGURE PREVUS SONT :
!          1) NOEUDS "2D"                DX, DY
!          2) NOEUDS "3D"                DX, DY, DZ
!          3) NOEUDS "POUTRE/COQUE 3D"   DX, DY, DZ, DRX, DRY, DRZ
!          4) NOEUDS "COQUE AXIS"        DX, DY, DRZ
!          LES AUTRES COMPOSANTES : LAGR, PRES, PHI, DDZDN, ...
!          NE SONT PAS "TOURNEES".
!
!     IN: NOMACR : NOM DU MACR_ELEM_STAT
!           L    : MATRICE DE CHANGEMENT DE REPERE (LOCAL -> GLOBAL)
!           SENS : 'LG' : LOCAL  -> GLOBAL
!                  'GL' : GLOBAL -> LOCAL
!          MATRIX: .TRUE. : MATRICE SYMETRIQUE
!                  .FALSE.: VECTEUR
!          TYPNOE N'EST UTILISE QUE SI MATRIX=.FALSE.
!          TYPNOE: 'EXTE' : UNIQUEMENT LES NOEUDS EXTERNES
!                  'TOUS' : TOUS LES NOEUDS (INTERNES ET EXTERNES)
!
!          SI "MATRIX"     , ON TOURNE K_EE
!          SI "NOT.MATRIX" , ON TOURNE F_I ET F_E
!
!
!          NOMACR: NOM DU MACR_ELEM_STAT.
!          IADM1 : ADRESSE DANS ZR DE LA ZONE MEMOIRE INITIALE (M1)
!          IADM2 : ADRESSE DANS ZR DU "RESULTAT" TOURNE (M2)
!
!          ATTENTION : DANS LE CAS .NOT.MATRIX :
!                      LES 2 ZONES MEMOIRES ZR(IADM1) ET ZR(IADM2)
!                      DOIVENT ETRE DIMENSIONNEES A NDDLI+NDDLE
!                      (POUR "TOUS" ET AUSSI POUR "EXTE" !!)
!
!          ATTENTION : LES 2 ADRESSES IADM1 ET IADM2 DOIVENT ETRE
!                      DIFFERENTES : PAS DE CALCUL "EN PLACE" POSSIBLE.
!
!
!     OUT:
!          LE VECTEUR A L'ADRESSE IADM2 EST CALCULE.
!-----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: long
    character(len=2) :: sens2
    integer :: i, i1, iacagd, iaiino, icmp
    integer :: icmpp, icumul, ieq, ieqdeb, ieqp, ier
    integer :: ii, ino, iret, j, j1, jj, k
    integer :: n1, nbno, nddle, nddli, nddlt, nulag, nuno
    integer :: nunold, nunop
    integer :: di, dj, dmi, dmj
    real(kind=8) :: p1(10, 10), p2(10, 10), l2(6, 6), li(10, 10), lj(10, 10)
    integer, pointer :: deeq(:) => null()
    integer, pointer :: desm(:) => null()
!-----------------------------------------------------------------------
!
!     P1 ET P2 MATRICES DE TRAVAIL.
!
!     FONCTIONS FORMULES:
!     -------------------
#define m1(i,j) iadm1-1+(j-1)*(j)/2+i
#define m2(i,j) iadm2-1+(j-1)*(j)/2+i
#define m1t(i,j) iadm1-1+(i-1)*(i)/2+j
#define v1(i) iadm1-1+i
#define v2(i) iadm2-1+i
!-----------------------------------------------------------------------
!
    call jemarq()
    sens2=sens
!
!
!     -1 VERIFICATION DE LA GRANDEUR "DEPLACEMENT" ET CALCUL DE NULAG
!     -------------------------------------------------------------
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'DEPL_R'), 'L', iacagd)
    ier=0
    if (indik8(zk8(iacagd),'DX',1,1) .ne. 1) ier=ier+1
    if (indik8(zk8(iacagd),'DY',1,2) .ne. 2) ier=ier+1
    if (indik8(zk8(iacagd),'DZ',1,3) .ne. 3) ier=ier+1
    if (indik8(zk8(iacagd),'DRX',1,4) .ne. 4) ier=ier+1
    if (indik8(zk8(iacagd),'DRY',1,5) .ne. 5) ier=ier+1
    if (indik8(zk8(iacagd),'DRZ',1,6) .ne. 6) ier=ier+1
    if (ier .gt. 0) then
        call utmess('F', 'SOUSTRUC_73')
    endif
    call dismoi('NU_CMP_LAGR', 'DEPL_R', 'GRANDEUR', repi=nulag)
    if (nulag .eq. 0) then
        call utmess('F', 'SOUSTRUC_74')
    endif
!
!
!     -2 CALCUL DE L2 = L OU LT :
!     ------------------------
    if (sens2 .eq. 'LG') then
        do i = 1, 6
            do j = 1, 6
                l2(i,j)=l(i,j)
            end do
        end do
    else if (sens2.eq.'GL') then
        do i = 1, 6
            do j = 1, 6
                l2(i,j)=l(j,i)
            end do
        end do
    else
        call utmess('F', 'SOUSTRUC_75', sk=sens2)
    endif
!
!
!     -3 2CALCUL DE NDDLI, NDDLE, NDDLT ET LONG :
!     --------------------------------
    call jeveuo(nomacr//'.DESM', 'L', vi=desm)
    call jeveuo(nomacr//'      .NUME.DEEQ', 'L', vi=deeq)
    nddle=desm(4)
    nddli=desm(5)
    nddlt= nddli+nddle
    if (matrix) then
        long= nddle*(nddle+1)/2
        ieqdeb=nddli+1
    else
        ASSERT((typnoe.eq.'TOUS').or.(typnoe.eq.'EXTE'))
        if (typnoe .eq. 'TOUS') then
            long= nddlt
            ieqdeb=1
        else if (typnoe.eq.'EXTE') then
            long= nddle
            ieqdeb=nddli+1
        endif
    endif
!
!
!     -4 RECUPERATION DE L'OBJET .IINO :
!     -------------------------------
!     .IINO(1) = NUMERO D'EQUATION DU 1ER CMP DU NOEUD 1
!     .IINO(2) = NOMBRE D'EQUATIONS DU NOEUD 1 (DI(1))
!     .IINO(3) = NOMBRE D'EQUATIONS CHANGEES PAR LA ROTATION (DMI(1))
!       (CES EQUATIONS SONT SENSEES SE SUIVRE ET ETRE PLACEES EN TETE)
!       VALEURS POSSIBLES : 2, 3, OU 6
!     .IINO(3+1) = NUMERO D'EQUATION DU 1ER CMP DU NOEUD 2
!     .IINO(3+2) = NOMBRE D'EQUATIONS DU NOEUD 2  (DI(2))
!     ....
!
    call jeexin('&&SSVARO.IINO', iret)
    if (iret .gt. 0) then
        call jelira('&&SSVARO.IINO', 'LONMAX', n1)
        if (n1 .lt. nddlt) then
            call jedetr('&&SSVARO.IINO')
            call wkvect('&&SSVARO.IINO', 'V V I', 3*nddlt, iaiino)
        else
            call jeveuo('&&SSVARO.IINO', 'E', iaiino)
        endif
    else
        call wkvect('&&SSVARO.IINO', 'V V I', 3*nddlt, iaiino)
    endif
!
!
!     -5 CALCUL DE .IINO :
!     --------------------
    nunold=0
    ino=0
    do ieq = ieqdeb, nddlt
        nuno =deeq(2*(ieq-1)+1)
        icmp=deeq(2*(ieq-1)+2)
        if ((icmp.le.0) .or. (icmp.eq.nulag)) then
!         --NOEUD DE LAGRANGE:
            ino= ino+1
            zi(iaiino-1+3*(ino-1)+1)=ieq-(ieqdeb-1)
            zi(iaiino-1+3*(ino-1)+2)=1
            zi(iaiino-1+3*(ino-1)+3)=0
            goto 2
        endif
        if (nuno .eq. nunold) goto 2
        nunold=nuno
        ino= ino+1
        zi(iaiino-1+3*(ino-1)+1)=ieq-(ieqdeb-1)
        if (icmp .eq. 1) then
            icumul=1
!       --ICUMUL COMPTE LA SOMME DES COMPOSANTES PRESENTES SUR LE NOEUD
        else
            call utmess('F', 'SOUSTRUC_76')
        endif
!
        dmi=0
        do ieqp = ieq+1, nddlt
            nunop =deeq(2*(ieqp-1)+1)
            if (nunop .ne. nuno) goto 22
            icmpp=deeq(2*(ieqp-1)+2)
            icumul=icumul+icmpp
!
!         -- CAS "2D"
!             3 = DX + DY
            if (icmpp .eq. 2) then
                dmi=2
                if (icumul .ne. 3) then
                    call utmess('F', 'SOUSTRUC_77')
                endif
            endif
!
!         -- CAS "3D"
!             6 = DX + DY + DZ
            if (icmpp .eq. 3) then
                dmi=3
                if (icumul .ne. 6) then
                    call utmess('F', 'SOUSTRUC_77')
                endif
            endif
!
!         -- CAS "POUTRE/COQUE 3D"
!             21 = DX + DY + DZ + DRX + DRY + DRZ
            if (icmpp .eq. 6) then
                if (icumul .eq. 21) then
                    dmi=6
!
!           -- CAS "POUTRE/COQUE AXIS"
!              9 = DX + DY + DRZ
                else if (icumul.eq.9) then
                    dmi=2
                else
                    call utmess('F', 'SOUSTRUC_77')
                endif
            endif
        end do
!
 22     continue
        di= ieqp-ieq
        if (di .gt. 10) ASSERT(.false.)
        zi(iaiino-1+3*(ino-1)+2)= di
        zi(iaiino-1+3*(ino-1)+3)= dmi
  2     continue
    end do
    nbno=ino
!
!
!     -6 RECOPIE DE M1 DANS M2 (POUR LES TERMES QUI NE TOURNENT PAS )
!        ("LAGR","PRES","DDNDZ","PHI",...  )
!     ----------------------------------------------------------------
    if (matrix) then
        do k = 1, long
            zr(iadm2-1+k)=zr(iadm1-1+k)
        end do
    else
        do k = ieqdeb, nddlt
            zr(iadm2-1+k)=zr(iadm1-1+k)
        end do
    endif
!
!
!     -7 CALCUL DES TERMES QUI TOURNENT (POUR UNE MATRICE) :
!     ------------------------------------------------------
    if (matrix) then
!
!     -7-1 CALCUL DE M2(I,J)= LIT*M1(I,J)*LJ:
!        (I ET J SONT DES NOEUDS !)
!     -----------------------------------------
        do j = 1, nbno
            j1= zi(iaiino-1+3*(j-1)+1)
            dj= zi(iaiino-1+3*(j-1)+2)
            dmj= zi(iaiino-1+3*(j-1)+3)
!
!       -7-2 CALCUL DE LJ :
!       -----------------
            do ii = 1, dj
                do jj = 1, dj
                    lj(ii,jj)=0.0d0
                end do
            end do
            do jj = 1, dj
                lj(jj,jj)=1.0d0
            end do
            do ii = 1, dmj
                do jj = 1, dmj
                    lj(ii,jj)=l2(ii,jj)
                end do
            end do
!
            do i = 1, j
                i1= zi(iaiino-1+3*(i-1)+1)
                di= zi(iaiino-1+3*(i-1)+2)
                dmi= zi(iaiino-1+3*(i-1)+3)
!
!         -7-3 CALCUL DE LI :
!         -----------------
                do ii = 1, di
                    do jj = 1, di
                        li(ii,jj)=0.0d0
                    end do
                end do
                do jj = 1, di
                    li(jj,jj)=1.0d0
                end do
                do ii = 1, dmi
                    do jj = 1, dmi
                        li(ii,jj)=l2(ii,jj)
                    end do
                end do
!
!         -7-4 RECOPIE DE M1(I,J) DANS P1:
!         ------------------------------
                if (i .lt. j) then
                    do jj = 1, dj
                        do ii = 1, di
                            p1(ii,jj)=zr(m1(i1-1+ii,j1-1+jj))
                        end do
                    end do
                else
                    do jj = 1, dj
                        do ii = 1, jj
                            p1(ii,jj)=zr(m1(i1-1+ii,j1-1+jj))
                        end do
                        do ii = jj+1, di
                            p1(ii,jj)=zr(m1t(i1-1+ii,j1-1+jj))
                        end do
                    end do
                endif
!
!         -7-5 P2=P1*L2(J):
!         --------------
                do ii = 1, di
                    do jj = 1, dj
                        p2(ii,jj)=0.0d0
                        do k = 1, dj
                            p2(ii,jj)=p2(ii,jj)+p1(ii,k)*lj(k,jj)
                        end do
                    end do
                end do
!
!         -7-6 P1=LT2(I)*P2:
!         ---------------
                do ii = 1, di
                    do jj = 1, dj
                        p1(ii,jj)=0.0d0
                        do k = 1, di
                            p1(ii,jj)=p1(ii,jj)+li(k,ii)*p2(k,jj)
                        end do
                    end do
                end do
!
!         -7-7 RECOPIE DE P1 DANS M2(I,J):
!         ------------------------------
                if (i .lt. j) then
                    do jj = 1, dj
                        do ii = 1, di
                            zr(m2(i1-1+ii,j1-1+jj))=p1(ii,jj)
                        end do
                    end do
                else
                    do jj = 1, dj
                        do ii = 1, jj
                            zr(m2(i1-1+ii,j1-1+jj))=p1(ii,jj)
                        end do
                    end do
                endif
!
!
            end do
        end do
    endif
!
!
!
!     -8 CALCUL DES TERMES QUI TOURNENT (POUR UN VECTEUR) :
!     ------------------------------------------------------
!
    if (.not.matrix) then
!
!     -- CALCUL DE V2(I)= L2T(I)*V1(I)
!        (I EST UN NOEUD !)
!     -----------------------------------------
        do i = 1, nbno
            i1= zi(iaiino-1+3*(i-1)+1) + (ieqdeb-1)
            dmi= zi(iaiino-1+3*(i-1)+3)
            do ii = 1, dmi
                zr(v2(i1-1+ii))=0.0d0
                do k = 1, dmi
                    zr(v2(i1-1+ii))=zr(v2(i1-1+ii))+l2(k,ii)*zr(v1(i1-1+k))
                end do
            end do
        end do
    endif
!
!
!
    call jedema()
end subroutine
