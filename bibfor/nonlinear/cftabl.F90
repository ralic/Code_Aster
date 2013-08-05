subroutine cftabl(indic, nbliac, ajliai, spliai, llf,&
                  llf1, llf2, resoco, typope, posit,&
                  iliai, typlia)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: indic
    integer :: nbliac
    integer :: ajliai
    integer :: spliai
    integer :: llf
    integer :: llf1
    integer :: llf2
    integer :: posit
    integer :: iliai
    character(len=1) :: typope
    character(len=2) :: typlia
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! MISE A JOUR DES VECTEURS DE LIAISONS CONTACT ET/OU FROTTEMENT
! LE NOMBRE DE LIAISONS EST MIS A JOUR DANS LA ROUTINE
!
!
! ----------------------------------------------------------------------
!
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
!                'A' : AJOUTER UNE LIAISON
!                'S' : SUPPRIMER UNE LIAISON
! IN  POSIT  : POSITION POUR AJOUTER UNE LIAISON DANS LE
!              VECTEUR DES LIAISONS ACTIVES
! IN  ILIAI  : INDICE DE LA LIAISON A AJOUTER OU SUPPRIMER
! IN  TYPLIA : TYPE DE LA LIAISON
!                'C0': CONTACT
!                'F0': FROTTEMENT (2D) OU FROTTEMENT SUIVANT LES DEUX
!                  DIRECTIONS SIMULTANEES (3D)
!                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
!                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
!
!
!
!
!
    integer :: ii, liaisp, posit2
    character(len=1) :: typeaj, typesp
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liac, typl
    integer :: jliac, jtypl
! ======================================================================
    call jemarq()
! ======================================================================
! --- APPEL JEVEUX POUR LA MISE A JOUR DES VECTEURS DE LIAISONS --------
! ======================================================================
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    call jeveuo(liac, 'E', jliac)
    call jeveuo(typl, 'E', jtypl)
! ======================================================================
! --- INITIALISATION DES VARIABLES TYPE DE CONTACT ---------------------
! ======================================================================
    typeaj = 'A'
    typesp = 'S'
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
! ======================================================================
    if (typope .eq. typeaj) then
! ======================================================================
! --- ON AJOUTE UNE LIAISON --------------------------------------------
! ======================================================================
        indic = 1
        zk8(jtypl-1+posit) = typlia
        zi (jliac-1+posit) = iliai
        if (typlia .eq. typec0) then
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
            nbliac = nbliac + 1
        else if (typlia.eq.typef0) then
! ======================================================================
! --- LIAISON DE FROTTEMENT ADHERENT (DEUX DIRECTIONS) -----------------
! ======================================================================
            llf = llf + 1
            do 111 ii = 1, nbliac + llf +llf1 + llf2 - 1
                if (zi(jliac-1+ii) .eq. iliai) goto 112
111          continue
            ASSERT(.false.)
112          continue
        else if (typlia.eq.typef1) then
! ======================================================================
! --- LIAISON DE FROTTEMENT ADHERENT (1ERE DIRECTION ) -----------------
! ======================================================================
            llf1 = llf1 + 1
        else if (typlia.eq.typef2) then
! ======================================================================
! --- LIAISON DE FROTTEMENT ADHERENT (2EME DIRECTION ) -----------------
! ======================================================================
            llf2 = llf2 + 1
        endif
! ======================================================================
    else if (typope.eq.typesp) then
! ======================================================================
! --- ON SUPPRIME UNE LIAISON ------------------------------------------
! ======================================================================
        liaisp = 0
        indic = -1
        if (typlia .eq. typec0) then
! ======================================================================
! --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE CONTACT ----------------
! --- ON SUPPRIME LA LIAISON DE FROTTEMENT ADHERENT ASSOCIEE -----------
! ======================================================================
            posit2 = nbliac + llf + llf1 + llf2 + 1
            nbliac = nbliac - 1
            ajliai = ajliai - 1
            do 10 ii = posit + 1, nbliac + llf + llf1 + llf2 + 1
                if (zi(jliac-1+ii) .eq. iliai) then
                    ajliai = ajliai - 1
                    posit2 = ii
                    liaisp = 1
                    if (zk8(jtypl-1+ii) .eq. typef0) then
! ======================================================================
! --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF ------------------
! ======================================================================
                        llf = llf - 1
                    else if (zk8(jtypl-1+ii).eq.typef1) then
! ======================================================================
! --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF1 -----------------
! ======================================================================
                        llf1 = llf1 - 1
                    else if (zk8(jtypl-1+ii).eq.typef2) then
! ======================================================================
! --- LA LIAISON ASSOCIEE EST UNE LIAISON DE TYPE LLF2 -----------------
! ======================================================================
                        llf2 = llf2 - 1
                    endif
                    goto 20
                endif
10          continue
20          continue
            do 30 ii = posit, (posit2-1) - 1
                zi (jliac-1+ii) = zi (jliac-1+ii+1)
                zk8(jtypl-1+ii) = zk8(jtypl-1+ii+1)
30          continue
            do 40 ii = posit2 - 1, nbliac + llf + llf1 + llf2
                zi (jliac-1+ii) = zi (jliac-1+ii+1+liaisp)
                zk8(jtypl-1+ii) = zk8(jtypl-1+ii+1+liaisp)
40          continue
        else
            ajliai = ajliai - 1
            if (typlia .eq. typef0) then
! ======================================================================
! --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF--
! ======================================================================
                llf = llf - 1
            else if (typlia.eq.typef1) then
! ======================================================================
! --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF1-
! ======================================================================
                llf1 = llf1 - 1
            else if (typlia.eq.typef2) then
! ======================================================================
! --- LA LIAISON A SUPPRIMER EST UNE LIAISON DE FROTTEMENT DE TYPE LLF2-
! ======================================================================
                llf2 = llf2 - 1
            endif
            do 50 ii = posit, nbliac + llf + llf1 + llf2
                zi (jliac-1+ii) = zi (jliac-1+ii+1)
                zk8(jtypl-1+ii) = zk8(jtypl-1+ii+1)
50          continue
        endif
    endif
! ======================================================================
! --- MISE A JOUR DE L'INDICATEUR POUR LA FACTORISATION DE LA MATRICE --
! --- DE CONTACT -------------------------------------------------------
! ======================================================================
    spliai = min(spliai,posit-1)
    spliai = min(spliai,nbliac+llf+llf1+llf2)
    if (ajliai .lt. 0) ajliai = 0
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
