subroutine modsta(motcle, matfac, matpre, solveu, lmatm,&
                  nume, iddl, coef, neq, nbmode,&
                  zrmod)
    implicit none
#include "jeveux.h"
#include "asterfort/ddllag.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/pteddl.h"
#include "asterfort/resoud.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: lmatm, iddl(*), neq, nbmode
    real(kind=8) :: coef(*), zrmod(neq, *)
    character(len=*) :: motcle, nume, matfac, matpre, solveu
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     CALCUL DE MODES STATIQUES
!
!     SI MOTCLE = 'DEPL' : CALCUL DE MODES CONTRAINTS
!                                    ( DEPLACEMENT UNITAIRE )
!                          LE TABLEAU IDDL EST CELUI DES NOEUDS BLOQUES
!                          ON APPLIQUE UNE FORCE UNITAIRE SUR LES LAGR
!     SI MOTCLE = 'FORC' : CALCUL DE MODES D'ATTACHE
!                                    ( FORCE UNITAIRE )
!                          LE TABLEAU IDDL EST CELUI DES NOEUDS ACTIFS
!     SI MOTCLE = 'ACCE' : CALCUL DE DEFORMEES STATIQUES
!                                    ( ACCELERATION VECTEUR UNITAIRE )
!     SI MOTCLE = 'ACCD' : CALCUL DE DEFORMEES STATIQUES
!                                    ( ACCELERATION DDL UNITAIRE )
!-----------------------------------------------------------------------
!  IN  : MOTCLE : CALCUL DE MODES CONTRAINTS OU D'ATTACHE
!  IN  : MATFAC : MATRICE DE RAIDEUR FACTORISEE
!  IN  : MATPRE : MATRICE DE PRECONDIONNEMENT POUR LA RAIDEUR (GCPC)
!  IN  : LMATM  : POINTEUR SUR LE DESCRIPTEUR DE LA MATRICE DE MASSE
!  IN  : NUME   : NOM DU NUME_DDL
!  IN  : IDDL   : TABLEAU DES DDL
!                 IDDL(I) = 0  PAS DE CALCUL DU MODE
!                 IDDL(I) = 1  CALCUL DU MODE
!  IN  : COEF   : COEFFICIENTS A APPLIQUER
!  IN  : NEQ    : NOMBRE D'EQUATIONS DU NUME
!  IN  : NBMODE : NOMBRE DE MODES STATIQUES
!  OUT : ZRMOD  : TABLEAU DES MODES STATIQUES CALCULES
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    real(kind=8) :: un
    character(len=8) :: nomcmp(3)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ic, ie, ila1, ila2, im, imod, in
    integer :: in2, ind, jddl, jddr
    integer :: iret
!-----------------------------------------------------------------------
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
!     ------------------------------------------------------------------
    call jemarq()
    un = 1.d0
    imod = 0
!
    if (motcle(1:4) .eq. 'ACCE') then
        call wkvect('&&MODSTA.POSITION_DDL', 'V V I', 3*neq, jddl)
        call pteddl('NUME_DDL', nume, 3, nomcmp, neq,&
                    zi(jddl))
        do 10 im = 1, nbmode
            imod = imod + 1
            in2 = 3 * ( im - 1 )
            call wkvect('&&MODSTA.POSITION_DDR', 'V V R', neq, jddr)
            do 12 ic = 1, 3
                ind = neq * ( ic - 1 )
                do 14 in = 0, neq-1
                    zr(jddr+in) = zr(jddr+in) + zi(jddl+ind+in) * coef(in2+ic)
14              continue
12          continue
            call mrmult('ZERO', lmatm, zr(jddr), zrmod(1, imod), 1,&
                        .true.)
            call jedetr('&&MODSTA.POSITION_DDR')
!
10      continue
        call jedetr('&&MODSTA.POSITION_DDL')
    else
        do 20 ie = 1, neq
            if (iddl(ie) .eq. 1) then
                imod = imod + 1
                if (motcle(1:4) .eq. 'DEPL') then
                    call ddllag(nume, ie, neq, ila1, ila2)
                    if (ila1 .eq. 0 .or. ila2 .eq. 0) then
                        call u2mess('F', 'ALGELINE2_4')
                    endif
                    zrmod(ila1,imod) = un
                    zrmod(ila2,imod) = un
                else if (motcle(1:4) .eq. 'FORC') then
                    zrmod(ie,imod) = un
                else
                    call wkvect('&&MODSTA.POSITION_DDR', 'V V R', neq, jddr)
                    call ddllag(nume, ie, neq, ila1, ila2)
                    if (ila1 .eq. 0 .or. ila2 .eq. 0) then
                        call u2mess('F', 'ALGELINE2_4')
                    endif
                    zr(jddr+ila1-1) = un
                    zr(jddr+ila2-1) = un
                    call resoud(matfac, matpre, solveu, ' ', 1,&
                                ' ', ' ', ' ', zr(jddr), cbid,&
                                ' ', .true., 0, iret)
                    call mrmult('ZERO', lmatm, zr(jddr), zrmod(1, imod), 1,&
                                .true.)
                    call jedetr('&&MODSTA.POSITION_DDR')
                endif
            endif
20      continue
    endif
!
!     --- RESOLUTION ---
    if (imod .gt. 0) then
        call resoud(matfac, matpre, solveu, ' ', imod,&
                    ' ', ' ', ' ', zrmod, cbid,&
                    ' ', .true., 0, iret)
    endif
    call jedema()
end subroutine
