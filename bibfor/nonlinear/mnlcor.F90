subroutine mnlcor(imat, numdrv, matdrv, xcdl, parcho,&
                  adime, ninc, nd, nchoc, h,&
                  hf, itemax, epscor, xvect, cor,&
                  info)
    implicit none
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE CORRECTION D'UN POINT SOLUTION
!     -    -   -                        ---
! ----------------------------------------------------------------------
!
! CORRIGE LE POINT DONNE EN ARGUMENT A L'AIDE D'UN ALGORITHME DE NEWTON
! ----------------------------------------------------------------------
! IN      IMAT       : I(2) : DESCRIPTEUR DES MATRICES :
!                              - IMAT(1) => MATRICE DE RAIDEUR
!                              - IMAT(2) => MATRICE DE MASSE
! IN      NUMDRV : K14  : NUME_DDL_GENE DE LA MATRICE JACOBIENNE
! IN      MATDRV : K19  : NOM DE  LA MATRICE JACOBIENNE
! IN      XCDL   : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN      PARCHO : K14  : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN      ADIME  : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN      NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN      ND     : I    : NOMBRE DE DEGRES DE LIBERTE ACTIFS
! IN      NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN      H      : I    : NOMBRE D'HARMONIQUES POUR LE DEPLACEMENT
! IN      HF     : I    : NOMBRE D'HARMONIQUES POUR LA FORCE
! IN      ITEMAX : I    : NOMBRE MAXIMAL D'ITERATIONS DE NEWTON
! IN      EPSCOR : R8   : PRECISION POUR L'ALGORITHME DE NEWTON
! IN/OUT  XVECT  : K14  : NOM DU VECTEUR D'ENTREE/VECTEUR CORRIGE
! OUT     COR    : L    : TRUE SI LA CORRECTION A REUSSI
!                         FALSE SINON
! IN      INFO   : I    : /1 --> PAS D'AFFICHAGE DE LA NORME D'ERREUR
!                         /2 -->       AFFICHAGE DE LA NORME D'ERREUR
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnldrv.h"
#include "asterfort/mnlru.h"
#include "asterfort/mnltan.h"
#include "asterfort/resoud.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dnrm2.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
!
    integer :: imat(2), ninc, nd, nchoc, h, hf, itemax, info
    character(len=14) :: numdrv, xcdl, parcho, adime, xvect
    character(len=19) :: matdrv
    real(kind=8) :: epscor
    aster_logical :: cor
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    character(len=14) :: xru, xtang, xtemp
    integer :: iru, itang, ivect, cptr, iret, itemp, inddl, ifres
    real(kind=8) :: eps, normr, normc
    complex(kind=8) :: cbid
    cbid = dcmplx(0.d0, 0.d0)
!
    ifres = iunifi ('MESSAGE')
!
    call jemarq()
! ----------------------------------------------------------------------
! --- CREATION DE VECTEURS UTILES
! ----------------------------------------------------------------------
    xru = '&&MNLCOR.RU'
    xtang = '&&MNLCOR.TANG'
    xtemp = '&&MNLCOR.TEMP'
    call wkvect(xru, 'V V R', ninc, iru)
    call wkvect(xtang, 'V V R', ninc, itang)
    call wkvect(xtemp, 'V V R', ninc, itemp)
    eps=epscor
! ----------------------------------------------------------------------
! --- RECUPERATION DU VECTEUR A CORRIGER
! ----------------------------------------------------------------------
    call jeveuo(xvect, 'E', ivect)
! ----------------------------------------------------------------------
! --- HEURISTIQUE SUR LE CHOIX DE L'EPS
! ----------------------------------------------------------------------
    call jeveuo(parcho//'.NDDL', 'L', inddl)
! ----------------------------------------------------------------------
! --- INITIALISATION DE L'ALGORITHME DE NEWTON
! ----------------------------------------------------------------------
    cptr = 0
    call mnlru(imat, xcdl, parcho, adime, xvect,&
               ninc, nd, nchoc, h, hf,&
               xru)
    zr(iru-1+ninc) = 0.d0
    normr = dnrm2(ninc-1,zr(iru),1)
    call dcopy(ninc, zr(ivect), 1, zr(itemp), 1)
    normc = normr
    900 format (' Norme erreur iteration Newton numero : ',i2,' : ',1pe12.5)
    if (info .eq. 2) then
        write(ifres,900) cptr,normc
    endif
! ----------------------------------------------------------------------
! --- ALGORITHME DE NEWTON
! ----------------------------------------------------------------------
120 continue
    if (normc .ge. eps .and. cptr .lt. itemax) then
! ---   INCREMENTATION DU COMPTEUR
        cptr = cptr+1
! ---   CALCUL DU VECTEUR TANGENT
        if (cptr .eq. 1) then
            call mnltan(.true._1, imat, numdrv, matdrv, xcdl,&
                        parcho, adime, xtemp, ninc, nd,&
                        nchoc, h, hf, xtang)
        else
            call mnltan(.false._1, imat, numdrv, matdrv, xcdl,&
                        parcho, adime, xtemp, ninc, nd,&
                        nchoc, h, hf, xtang)
        endif
! ---   RECALCUL DE LA MATRICE JACOBIENNE
        call mnldrv(.true._1, imat, numdrv, matdrv, xcdl,&
                    parcho, adime, xtemp, zr(itang), ninc,&
                    nd, nchoc, h, hf)
! ---   ON RESOUD LE SYSTEME LINEAIRE (DRDV\XTANG)
        call resoud(matdrv, ' ', ' ', ' ', 1,&
                    ' ', ' ', 'V', zr(iru), [cbid],&
                    ' ', .false._1, 0, iret)
! ---   ON AJOUTE AU VECTEUR SOLUTION
        call daxpy(ninc, -1.d0, zr(iru), 1, zr(itemp),&
                   1)
! ---   ON CALCUL R(NOUVEAU VECTEUR SOLUTION)
        call mnlru(imat, xcdl, parcho, adime, xtemp,&
                   ninc, nd, nchoc, h, hf,&
                   xru)
        zr(iru-1+ninc) = 0.d0
! ---   ON CALCUL LA NORME DE R(NOUVEAU VECTEUR SOLUTION)
        normc = dnrm2(ninc-1,zr(iru),1)
        normr=normc
        call dcopy(ninc, zr(itemp), 1, zr(ivect), 1)
        if (info .eq. 2) then
            write(ifres,900) cptr,normc
        endif
        goto 120
    endif
    if (normr .ge. eps) then
        cor=.false.
    else
        cor=.true.
    endif
! ----------------------------------------------------------------------
! --- DESTRUCTION DES VECTEURS UTILES
! ----------------------------------------------------------------------
    call jedetr(xru)
    call jedetr(xtang)
    call jedetr(xtemp)
!
    call jedema()
!
end subroutine
