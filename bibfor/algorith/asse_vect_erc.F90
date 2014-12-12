subroutine asse_vect_erc(baseno,nom_vect_erc,nommes,matobs, obsdim, alpha,n_ordre_mes,omega)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  ROUTINE DE REMPLISSAGE DES VALEURS DU SECOND MEMBRE ERC EN MODALE
!  ON CONSTRUIT LE PRODUIT coef_alpha*H^T*G*tilde(u)
! ----------------------------------------------------------------------
! IN  : BASENO        : NOM COMMUN POUR LES OBJETS JEVEUX A CREER
! IN  : NOM_VECT_ERC  : NOM DE L'OBJET JEVEUX DU 2ND MEMBRE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! IN  : NOM_NUME_ERC  : NOM DE L'OBJET JEVEUX DU NUME_DDL CREE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! IN  : NOMMES        : NOM DU CONCEPT JEVEUX CONTENANT LA MESURE
! IN  : MATOBS        : LISTE DES NOMS DES OBJETS JEVEUX DEFINISSANT LA MATRICE
!                       D'OBSERVATION. LA MATRICE EST STOCKEE EN SPARSE SOUS LE
!                       FORMAT COO (FILE,COLONNE,VALEUR)
! IN  : OBSDIM        : TABLEAU DONNANT LES INFORMATIONS DIMENSIONNELLES DE LA
!                       MATRICE D'OBSERVATION (DIM_FILE,DIM_COLONNE,NOMBRE_DE_
!                       VALEURS_NONNULLES)
! IN  : ALPHA         : PARAMETRE ALPHA DE LA FONCTIONNELLE D'ERC
! IN  : N_ORDRE_MES   : NUMERO D'ORDRE DE LA MESURE ASSOCIE A LA FREQ EN COURS
! IN  : OMEGA         : PULSATION ASSOCIEE A LA FREQUENCE EN COURS
! ----------------------------------------------------------------------!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/mrmult.h"
#include "asterfort/r8inir.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
#include "asterfort/jedetr.h"
#include "blas/dcopy.h"
#include "blas/dspmv.h"
!
    character(len=4) :: type_mes
    character(len=8),intent(in) :: nommes,baseno
    character(len=8) :: mnorme
    character(len=11) :: bl11
    character(len=19),intent(in) :: nom_vect_erc
    character(len=19) :: nom_objev_mes
    integer,intent(in) :: obsdim(3),n_ordre_mes
    integer :: occ,i_tach,i_mes,ii,idesc,nvect_mes,nvale_norme,ivale_norm
    integer :: iaux1,iaux2,iobsfil,iobscol,iobsval,ivecterc,n_fil,n_col
    real(kind=8),intent(in) :: alpha,omega
    real(kind=8) :: coeff_alpha,coef_mes
    character(len=24),intent(in) :: matobs(3)
    logical :: isdiag
!    
    bl11 = '           '

! --- RECUPERATION DE LA MESURE
    call getvtx(' ', 'CHAMP_MESURE',scal=type_mes)
    if (type_mes.eq.'DEPL') then
       occ=1
       coef_mes=1.0d0
    else if (type_mes.eq.'VITE') then
       occ=2
       coef_mes=1.0d0/omega
    else 
       occ=3
       coef_mes=1.0d0/(-omega*omega)
    end if

    call jeveuo(jexnum(nommes//'           .TACH', occ), 'L', i_tach)
    nom_objev_mes=zk24(i_tach+n_ordre_mes-1)(1:19)
    call jeveuo(nom_objev_mes//'.VALE', 'L', i_mes)
!

    isdiag=.true.
! --- RECUPERATION DE LA MATRICE NORME
    call getvid(' ', 'MATR_NORME',scal=mnorme)
    call jeveuo(mnorme//bl11//'.DESC', 'L', idesc)
    nvect_mes=zi(idesc+1)
    nvale_norme=nvect_mes
    if (zi(idesc+2) .eq. 2) isdiag=.false.
    if (.not.isdiag) nvale_norme=(nvect_mes*(nvect_mes+1))/2
!
    if (nvect_mes.ne.obsdim(1)) then
       call utmess('F', 'ALGORITH9_71')
    endif
!
    call jeveuo(jexnum(mnorme//bl11//'.VALM', 1), 'L', ivale_norm)
! 
! --- PRODUIT   coef_alpha*G*tilde(u)
    coeff_alpha=-2.0d0*alpha/(1.0d0-alpha)*coef_mes
! --- ON RECOPIE LA MESURE DANS UN VECTEUR DE TRAVAIL iaux1
    call wkvect(baseno//'.VECAUX1ERC.VAL', 'V V R', nvect_mes, iaux1)
    call dcopy(nvect_mes,zr(i_mes),1,zr(iaux1),1)
! --- ON CREE UN (PETIT) VECTEUR DE TRAVAIL  iaux2                 
    call wkvect(baseno//'.VECAUX2ERC.VAL', 'V V R', nvect_mes, iaux2)
    call r8inir(nvect_mes,0.d0,zr(iaux2),1)
 
! --- PREMIER PRODUIT MATRICE VECTEUR   coef_alpha*G*tilde(u)

    if (isdiag) then
        
           do ii=1,nvect_mes
               zr(iaux2-1+ii)=coeff_alpha*zr(ivale_norm-1+ii)*zr(iaux1-1+ii)
           end do

    else
        call dspmv('u',nvect_mes,coeff_alpha,zr(ivale_norm),zr(iaux1),1,0.d0,zr(iaux2),1)
    end if
! --- RECUPERATION DE LA MATRICE D'OBSERVATION
    call jeveuo(matobs(1), 'L', iobsfil)
    call jeveuo(matobs(2), 'L', iobscol)
    call jeveuo(matobs(3), 'L', iobsval)
!   --- RECUPERATION ET PRECONDITIONNEMENT DU VECTEUR ERC
    call jeveuo(nom_vect_erc//'.VALE','L',ivecterc) 
    call r8inir(2*obsdim(2),0.d0,zr(ivecterc),1)

! --- FINALISATION DU PRODUIT   H^T* VECT_AUX_2 (coef_alpha*G*tilde(u))

    do ii=1,obsdim(3)
     n_fil=zi(iobsfil-1+ii)
     n_col=zi(iobscol-1+ii)
     zr(ivecterc-1+obsdim(2)+n_col)=zr(ivecterc-1+obsdim(2)+n_col) &
                                                                 +zr(iobsval-1+ii)*zr(iaux2-1+n_fil)
    end do
!     NETOYAGE DES OBJETS JEVEUX TEMPORAIRES
    call jedetr(baseno//'.VECAUX1ERC.VAL')
    call jedetr(baseno//'.VECAUX2ERC.VAL')

end subroutine