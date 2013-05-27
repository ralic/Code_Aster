subroutine xvoise(nnotot, nse, nnop, nno, jcnset,&
                  cninv, cvoise)
    implicit none
    include 'jeveux.h'
    integer :: nvois
    parameter (nvois=3)
    integer :: nnotot, nse, nnop, nno, jcnset
    integer :: cninv(nnotot, nse+1), cvoise(nvois, nse)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     BUT:
!         RECHERCHE DES VOISINS DES SOUS ELEMENTS DE
!         L'ELEMENT XFEM PARENT (EN 2D), PUIS ECRITURE DANS ZI()
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NDIM   : DIMENSION
! IN   NNOTOT : NOMBRE TOTAL DE NOEUDS (POINTS D'INTERSECTION INCLUS)
! IN   NSE    : NOMBRE TOTAL DE SOUS ELEMENT DE L'ELEMENT PARENT
! IN   NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT (POINTS)
!                D'INTERSECTION EXCLUS
! IN   NNO    : NOMBRE DE NOEUDS DU SOUS-ELEMENT DE REFERENCE
! IN   JCNSET : ADRESSE DANS ZI DE LA CONNECTIVITE DES SOUS-ELEMENTS
! IN   CNINV  : TABLEAU DE LA CONNECTIVITE INVERSE
!
!      SORTIE :
!-------------
! OUT  CVOISE : TABLEAU DES VOISINS PAR SOUS-ELEMENT
!
! ......................................................................
!
!
!
!
    integer :: nbmav1, nbmav2
    integer :: ise, in, ino, jno, imav1, numav1, indma1
    integer :: insui, inosui, jnosui, imav2, numav2, indma2
!
! ----------------------------------------------------------------------
! -----------------  BOUCLE SUR LES NSE SIMPLEXES  ------------------
! ----------------------------------------------------------------------
!
    do 210 ise = 1, nse
!
! --------------------------------------------------------------------
! ------------  BOUCLE SUR LES SOMMETS DU SOUS-ELEMENTS  -------------
! --------------------------------------------------------------------
!
        do 211 in = 1, nno
!
! ------- RECUPERATION DE LA NUMEROTATION XFEM
!
            ino=zi(jcnset-1+nno*(ise-1)+in)
!
! ------- NUMEROTATION PROPRE A LA CONNECTIVITE INVERSE
!
            if (ino .lt. 1000) then
                jno=ino
            else
                jno=ino-1000+nnop
            endif
!
            nbmav1=cninv(jno,1)
!
! --------------------------------------------------------------------
! -------  BOUCLE SUR LES VOISINS POTENTIELS CONTENANT "JNO"  --------
! --------------------------------------------------------------------
!
            do 212 imav1 = 1, nbmav1
!
                indma1=imav1+1
                numav1=cninv(jno,indma1)
!
! --------- ON S'ASSURE QUE LE VOISIN POTENTIEL N'EST PAS LE
! --------- SOUS-ELEMENT COURANT
!
                if (numav1 .ne. ise) then
!
!
! ----------- RESPECT DE LA NUMEROTATION AU SEIN DU SOUS-ELEMENT
!
                    if (in .eq. nno) then
                        insui=1
                    else
                        insui=in+1
                    endif
!
! ----------- RECUPERATION DE LA NUMEROTATION XFEM
!
                    inosui=zi(jcnset-1+nno*(ise-1)+insui)
!
! ----------- NUMEROTATION PROPRE A LA CONNECTIVITE INVERSE
!
                    if (inosui .lt. 1000) then
                        jnosui=inosui
                    else
                        jnosui=inosui-1000+nnop
                    endif
!
                    nbmav2=cninv(jnosui,1)
!
! --------------------------------------------------------------------
! ----  BOUCLE SUR LES VOISINS POTENTIELS CONTENANT "JNOSUI"  --------
! --------------------------------------------------------------------
!
                    do 213 imav2 = 1, nbmav2
!
                        indma2=imav2+1
                        numav2=cninv(jnosui,indma2)
!
! ------------- ON LOCALISE LE VOISIN SITUE EN VIS-À-VIS DE L'ARRETE
! ------------- [JNO,JNOSUI] (S'IL EXISTE), PUIS ON L'ECRIT DANS ZI()
!
                        if (numav2 .eq. numav1) then
                            cvoise(in,ise)=numav1
                        endif
!
213                  continue
!
                endif
!
212          continue
!
211      continue
!
210  end do
!
end subroutine
