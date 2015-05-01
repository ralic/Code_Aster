subroutine blkobs(matobs, obsdim,alpha,matprod)
!
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
!
    implicit none
!
! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  ON RECUPERE LA MATRICE NORME 'G' ET ON CONSTRUIT LE SOUS-BLOC : 
!
!                   coeff_alpha*H^T*G*H
!
!  ASSOCIE LA MATRICE D'OBSERVATION
!  
! ----------------------------------------------------------------------
!
! IN : MATOBS   : LISTE DES NOMS DES OBJETS JEVEUX DEFINISSANT LA MATRICE
!                 D'OBSERVATION. LA MATRICE EST STOCKEE EN SPARSE SOUS LE
!                 FORMAT COO (FILE,COLONNE,VALEUR)
! IN : OBSDIM   : TABLEAU DONNANT LES INFORMATIONS DIMENSIONNELLES DE LA
!                 MATRICE D'OBSERVATION (DIM_FILE,DIM_COLONNE,NOMBRE_DE_
!                 VALEURS_NONNULLES)
! IN : ALPHA    : PARAMETRE ALPHA DE LA FONCTIONNELLE D'ERC
! OUT : MATPROD : LISTE DES NOMS DES OBJETS JEVEUX OU EST STOCKE LE SOUS-
!                 BLOC CALCULE. LE STOCKAGE EST EN MORSE SELON LA 
!                 SD_NUME_DDL. (.SMDE,.SMHC,.SMDI,.VALM)
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/cntdif.h"
#include "blas/dpotrf.h"
#include "asterfort/getvid.h"
#include "asterfort/jacopo.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/r8inir.h"
#include "asterfort/wkvect.h"
#include "asterfort/indiis.h"
#include "asterfort/utmess.h"
#include "blas/dscal.h"
!
    integer,intent(in) :: obsdim(3)
    character(len=24),intent(in) :: matobs(3)
    real(kind=8),intent(in) :: alpha
    character(len=24),intent(out) :: matprod(4)
    character(len=8) :: baseno, mnorme
    character(len=11) :: bl11
    integer :: idesc, ivale, nvect,  nvale, iobfil, iobcol, iobval, ii, jj
    integer :: indval, info, inwfil, inwcol, inwval, ifile, ntnlmp, diff, kk
    integer :: valdif(obsdim(3)), ifull, numcol, numfil, nstock, ictcol, linfil(obsdim(1)+1), il1
    integer :: il2, posvec, taivec, nfil1, nfil2, tt, ismde, ismdi, ismhc, ivalm, aa, bb, icolst
    integer :: idiff, istock, ifil
    logical :: isdiag
    real(kind=8) :: MATCHOL(obsdim(1), obsdim(1)),coeff_alpha
!
    baseno='&&OP0066'
    bl11 = '           '
    isdiag=.true.
! --- RECUPERATION DE LA MATRICE NORME
    call getvid(' ', 'MATR_NORME',scal=mnorme)
    call jeveuo(mnorme//bl11//'.DESC', 'L', idesc)
    if (zi(idesc+2) .eq. 2) isdiag=.false.
    nvect=zi(idesc+1)
    nvale=nvect
    if (.not.isdiag) nvale=(nvect*(nvect+1))/2
    if (nvect.ne.obsdim(1)) then
       call utmess('F', 'ALGORITH9_65')
    endif
!
    call jeveuo(jexnum(mnorme//bl11//'.VALM', 1), 'L', ivale)
!
! --- DECOMPOSITION DE CHOLESKY DE LA NORME
! --- INITIALISATION DES VALEURS A ZERO DU TABLEAU
    do ii = 1, obsdim(1)
        do  jj = 1, obsdim(1)
            MATCHOL(ii,jj)=0.d0
        end do
    end do
!
    if (isdiag) then
        do  ii = 1, obsdim(1)
            MATCHOL(ii,ii)=zr(ivale+ii-1)
        end do
    else
        indval=0
        do jj = 1, nvect
            do  ii = 1, jj
                MATCHOL(ii,jj)=zr(ivale+indval)
                indval=indval+1
            end do
        end do
    endif
!     DECOMPOSITION DE CHOLESKY PROPREMENT DITE
    call dpotrf('U', nvect, MATCHOL, nvect, info)
!
! --- RECUPERATION DE LA MATRICE D'OBSERVATION
    call jeveuo(matobs(1), 'L', iobfil)
    call jeveuo(matobs(2), 'L', iobcol)
    call jeveuo(matobs(3), 'L', iobval)

!     !!
! --- PAR CONSTRUCTION LA MATRICE D'OBSERVATION VIENT RANGEE PAR FILE
!     CROISSANTE
!     !!
    if (isdiag) then
! ---  CAS OU LA MATRICE NORME EST DIAGONALE
! -    ON COPIE LA MATRICE PRODUIT DANS DES VECTEURS DE TRAVAIL
!      ET ON MULTIPLIE LES VALEURS PAR LA MATRICE NORME
        call wkvect(baseno//'.M.PROD.ERC.FIL', 'V V I', obsdim(3), inwfil)
        call wkvect(baseno//'.M.PROD.ERC.COL', 'V V I', obsdim(3), inwcol)
        call wkvect(baseno//'.M.PROD.ERC.VAL', 'V V R', obsdim(3), inwval)
        call jacopo(obsdim(3), 'I', iobfil, inwfil)
        call jacopo(obsdim(3), 'I', iobcol, inwcol)
!
        ifile=zi(iobfil)
        do  ii = 1, obsdim(3)
            if (zi(iobfil+ii-1) .ne. ifile) ifile=zi(iobfil+ii-1)
            zr(inwval+ii-1)=zr(iobval+ii-1)*MATCHOL(ifile,ifile)
        end do
!
! ---  CAS OU LA MATRICE NORME N'ES PAS DIAGONALE
!
    else
!
! ---  CALCUL DES TERMES NON-NULS DE LA MATRICE PRODUIT
        ntnlmp=0
        do  ii = 1, obsdim(1)
            kk=indiis(zi(iobfil),ii,1,obsdim(3))
            call cntdif(iobcol+kk-1, obsdim(3)-kk+1, diff, valdif, obsdim( 3))
            ntnlmp=ntnlmp+diff
        end do
!
! ---  NTNLMP EST UN COMPTEUR QUI INDIQUE LES TERMES NON-NULS
        call wkvect(baseno//'.M.PROD.ERC.FIL', 'V V I', ntnlmp, inwfil)
        call wkvect(baseno//'.M.PROD.ERC.COL', 'V V I', ntnlmp, inwcol)
        call wkvect(baseno//'.M.PROD.ERC.VAL', 'V V R', ntnlmp, inwval)
!
! ---  CONSTUCTION DU PRODUIT sqrt(G)*H
! ---  REMPLISSAGE DES VALEURS DES FILES ET COLONNES
        ifull=1
        do  ii = 1, obsdim(1)
            kk=indiis(zi(iobfil),ii,1,obsdim(3))
            call cntdif(iobcol+kk-1, obsdim(3)-kk+1, diff, valdif, obsdim( 3))
            do  jj = 1, diff
                zi(inwfil+ifull-1)=ii
                zi(inwcol+ifull-1)=valdif(jj)
                ifull=ifull+1
            end do
        end do
! ---  REMPLISSAGE DES VALEURS NON-NULLES DE LA MATRICE PROD
!      1.- INITIALISATION A ZERO
        call r8inir(ntnlmp, 0.d0, zr(inwval), 1)
!      BOUCLE SUR CHAQUE VALEUR NON-NULLE
        do  jj = 1, ntnlmp
!
            numfil=zi(inwfil+jj-1)
            numcol=zi(inwcol+jj-1)
!
!       ON CHERCHE L'INDICE DANS LA MATRICE H INTIALE CORRESPONDANT A
!       LA FILE EN COURS
            do  ii = 1, obsdim(1)
                kk=indiis(zi(iobcol),numcol,ii,obsdim(3))
                if (kk .eq. 0) goto 999
                if (zi(iobfil+kk-1) .ge. numfil) then
                    zr(inwval+jj-1)=zr(inwval+jj-1) +zr(iobval+kk-1)*&
                    MATCHOL(numfil,zi(iobfil+kk-1))
                endif
!
          end do
999         continue
!
       end do
!
    endif
!
! --- A CE STADE ON A CONSTRUIT LE PRODUIT sqrt(G)*H
!
! --- CONSTRUCTION DE (sqrt(G)*H)^T * (sqrt(G)*H) SOUS UNS STOCKAGE
!     DE TYPE NUME_DDL
!
!     1.-COMPTAGE DES TERMES STOCKES
!
!     TOUS LES TERMES DIAGONAUX (OBLIGATOIRE)
    nstock=obsdim(2)
!     COMPTAGE DES TERMES EXTRA-DIAG
    call wkvect(baseno//'.COUNT.ERC.COL', 'V V I', ntnlmp, ictcol)
    call cntdif(inwcol, ntnlmp, diff, zi(ictcol), ntnlmp)
!
!
!     CONSTRUCTION D'UNE LISTE D'INDICES POUR FACILITER LA TACHE
!     DE RECHERCHE DE COUPLES PAR FILE
    do  ii = 1, obsdim(1)
        linfil(ii)=indiis(zi(inwfil),ii,1,ntnlmp)
    end do
    linfil(obsdim(1)+1)=ntnlmp+1
!     RECHERCHE DES COUPLES EXISTANTS
    do    ii = 1, diff
        nfil1=zi(ictcol+ii-1)
        if ((ii+1) .le. diff) then
            do    jj = ii+1, diff
                nfil2=zi(ictcol+jj-1)
                do    tt = 1, obsdim(1)
                    posvec=linfil(tt)-1
                    taivec=linfil(tt+1)-linfil(tt)
                    il1=indiis(zi(inwcol+posvec),nfil1,1,taivec)
                    il2=indiis(zi(inwcol+posvec),nfil2,1,taivec)
                    if ((il1.ne.0) .and. (il2.ne.0)) goto 888
                end do
888             continue
                nstock=nstock+1
            end do
        endif
    end do
!
! --- CREATION VECTEURS DE LA BONNE TAILLE POUR STOCKAGE MORSE NUME_DDL
    
    matprod(1)=baseno//'NU.HTGH.ERC.SMDE'
    matprod(2)=baseno//'NU.HTGH.ERC.SMHC'
    matprod(3)=baseno//'NU.HTGH.ERC.SMDI'
    matprod(4)=baseno//'MA.HTGH.ERC.VALM'

    call wkvect(matprod(1), 'V V I', 3, ismde)
    zi(ismde)=obsdim(2)
    zi(ismde+1)=nstock
    zi(ismde+2)=1
    call wkvect(matprod(2), 'V V S', nstock, ismhc)
    call wkvect(matprod(3), 'V V I', obsdim(2), ismdi)
    call wkvect(matprod(4), 'V V R', nstock, ivalm)

! --- REMPLISSAGE
    call r8inir(nstock, 0.d0, zr(ivalm), 1)
!
    istock=1
    idiff=1
!     ON AVANCE COLONNE PAR COLONNE
    do    icolst = 1, obsdim(2)
!
!        SI LA COLONNE EN COURS CORRESPOND A UN TERME NON NUL IL FAUT
!        REMPLIR LES VALEURS DE LA COLONNE
        if (icolst .eq. zi(ictcol+idiff-1)) then
!         RECHERCHE (EN ARRIERE) DES COUPLES FILE,COLONNE PRESENTS
            do    aa = 1, idiff
                ifil=zi(ictcol+aa-1)
!
                do    bb = 1, obsdim(1)
                    posvec=linfil(bb)-1
                    taivec=linfil(bb+1)-linfil(bb)
                    il1=indiis(zi(inwcol+posvec),ifil,1,taivec)
                    il2=indiis(zi(inwcol+posvec),icolst,1,taivec)
!
                    if ((il1.ne.0) .and. (il2.ne.0)) then
                        zr(ivalm+istock-1)=zr(ivalm+istock-1)+&
                        zr(inwval+posvec+il1-1)*zr(inwval+posvec+il2-&
                        1)
!             CETTE VALEUR EST POTENTIELLEMENT RENSEIGNEE PLUSIEURS FOIS
                        zi4(ismhc+istock-1)=int(ifil,4)
                    endif
                end do
!
                istock=istock+1
            end do
            idiff=idiff+1
!
        else
!        CAS OU ON EST DANS UNE COLONNE AVEC DES ZEROS
!        LA VALEUR DE .VALM EST DIRECTEMENT OK ET ON RENSEIGNE .SMHC
!        IDENTIQUE A LA VALEUR DE LA COLONNE (ON EST SUR LA DIAG)
            zi4(ismhc+istock-1)=int(icolst,4)
            istock=istock+1
        endif
!
        zi(ismdi+icolst-1)=istock-1
!
    end do
!
! --- ON FINIT PAR FAIRE LE PRODUIT PAR LE COEFFICIENT RELATIF A ALPHA
      coeff_alpha=-2.0d0*alpha/(1.0d0-alpha)
      call dscal(nstock, coeff_alpha, zr(ivalm) , 1)


!
! --- A CE STADE LE PRODUIT  (H^T*G*H) EST FAIT ET STOCKE SELON LA
!     SD-STOCKAGE MORSE. LES VECTEURS SONT DANS LA VARIABLE DE SORTIE matprod
!
!     NETOYAGE DES OBJETS JEVEUX TEMPORAIRES
    call jedetr(baseno//'.COUNT.ERC.COL')
    call jedetr(baseno//'.M.PROD.ERC.FIL')
    call jedetr(baseno//'.M.PROD.ERC.COL')
    call jedetr(baseno//'.M.PROD.ERC.VAL')
!
!
end subroutine
