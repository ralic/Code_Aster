subroutine gabscu(lobj2, coorn, nomno, fond, xl,&
                  absgam)
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
!     ----------------------------------------------------------------
! FONCTION REALISEE:
!
!     POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON CALCULE
!     SON ABSCISSE CURVILIGNE
!     TRAITEMENT PARTICULIER SI LA COURBE EST FERMEE
!
!     ------------------------------------------------------------------
! ENTREE:
!        LOBJ2  : NOMBRE DE NOEUD DE GAMM0
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DES NOEUDS
!        NOMNO  : NOM DE L'OBJET CONTENANT LES NOMS DES NOEUDS
!        FOND   : NOMS DES NOEUDS DU FOND DE FISSURE
!
! SORTIE:
!        XL     : LONGUEUR DE LA FISSURE
!        ABSGAM : ABSCISSE CURVILIGNE DES NOEUDS DU FOND DE FISSURE
!     ------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: nomno, coorn, numgam, absgam, fond
!
    integer :: lobj2, iadrco, iadrno, iadnum, iadabs
!
    real(kind=8) :: xi1, yi1, zi1, xj1, yj1, zj1, xij, yij, zij, xl
!
!
!-----------------------------------------------------------------------
    integer :: i, iret, j
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(coorn, 'L', iadrco)
    call jeveuo(fond, 'L', iadrno)
!
! CALCUL DE LA LONGUEUR DU FOND DE FISSURE
!
! RECUPERATION DES NUMEROS DE NOEUDS DE GAMM0
!
!
    numgam = '&&LEGEND.NUMGAMM0'
    call wkvect(numgam, 'V V I', lobj2, iadnum)
    do 2 j = 1, lobj2
        call jenonu(jexnom(nomno, zk8(iadrno+j-1)), zi(iadnum+j-1))
 2  end do
!
    xl = 0.d0
    do 3 j = 1, lobj2-1
        xi1 = zr(iadrco+(zi(iadnum+j-1) -1)*3+1-1)
        yi1 = zr(iadrco+(zi(iadnum+j-1) -1)*3+2-1)
        zi1 = zr(iadrco+(zi(iadnum+j-1) -1)*3+3-1)
        xj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+1-1)
        yj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+2-1)
        zj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+3-1)
        xij = xj1-xi1
        yij = yj1-yi1
        zij = zj1-zi1
        xl = xl + sqrt(xij*xij + yij *yij +zij*zij)
 3  end do
!
!  CALCUL DE L'ABSCISSE CURVILIGNE DE CHAQUE NOEUD DE GAMM0
!
    absgam = '&&LEGEND.ABSGAMM0'
    call jeexin(absgam, iret)
    if (iret .eq. 0) then
        call wkvect(absgam, 'V V R', lobj2, iadabs)
!
        zr(iadabs) = 0.d0
        do 4 i = 1, lobj2-1
            xi1 = zr(iadrco+(zi(iadnum+i-1) -1)*3+1-1)
            yi1 = zr(iadrco+(zi(iadnum+i-1) -1)*3+2-1)
            zi1 = zr(iadrco+(zi(iadnum+i-1) -1)*3+3-1)
            xj1 = zr(iadrco+(zi(iadnum+i+1-1)-1)*3+1-1)
            yj1 = zr(iadrco+(zi(iadnum+i+1-1)-1)*3+2-1)
            zj1 = zr(iadrco+(zi(iadnum+i+1-1)-1)*3+3-1)
            xij = xj1-xi1
            yij = yj1-yi1
            zij = zj1-zi1
            zr(iadabs+i+1-1) = zr(iadabs+i-1)+ sqrt(xij*xij + yij * yij + zij*zij)
 4      continue
    endif
!
! DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr(numgam)
!
    call jedema()
end subroutine
