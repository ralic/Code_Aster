subroutine xoriff(info, nfon, jfono, jbaso, jtailo,&
                  nmafon, listpt, goinop, jfon, jbas,&
                  jtail, fonmul, nbfond)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/padist.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xffcr.h'
    include 'asterfort/xffext.h'
    integer :: nfon, jfono, jbaso, jtailo, nmafon, jfon, jbas, jtail
    integer :: nbfond
    character(len=19) :: info, listpt
    character(len=24) :: fonmul
    logical :: goinop
!
! ----------------------------------------------------------------------
!       ORIENTATION DES POINTS DU FOND DE FISSURE DANS LE CADRE DE XFEM
!
!  ENTRESS :
!     INFO  :   NOM DU VECTEUR INFO DE LA SD
!     JFONO :   ADRESSE DES POINTS DU FOND DE FISSURE DÉSORDONNÉS
!     JBASO :   ADRESSE DES DIRECTIONS DE PROPAGATION DÉSORDONNÉES
!     JTAILO:   ADRESSE DES TAILLES MAXIMALES DE MAILLES DÉSORDONNÉES
!     NFON  :   NOMBRE DE POINTS DU FOND DE FISSURE DÉSORDONNÉS
!     LISTPT:   LISTE DES INDICES DES POINTS DU FOND DÉSORDONNÉS
!     GOINOP :  .TRUE.  SI  OPOO10 AVEC UPWIND-SIMPLEXE/GRILLE/3D
!               .FALSE. SINON
!
!  SORTIES :
!     JFON  :  ADRESSE DES POINTS DU FOND DE FISSURE ORDONNÉS
!     JBAS  :  ADRESSE DES DIRECTIONS DE PROPAGATION ORDONNÉES
!     JTAIL :  ADRESSE DES TAILLES MAXIMALES DE MAILLES ORDONNÉES
!     FONMUL:  VECTEUR CONTENANT LE DEBUT ET L'ARRIVEE DE
!              CHAQUE FOND DE FISSURE
!     NBFOND:  NOMBRE DE FONDS DE FISSURE
!
!     ------------------------------------------------------------------
!
    integer :: indice, indicm, indipt, ima, ipt, iptext
    integer :: jfonmu, jinfo, jlistp, jptext, jtabpt, k, nbptex
    real(kind=8) :: absc, m(3), p(3)
    character(len=19) :: ptextr, tabpt, typfon
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     RECHERCHE DES POINTS EXTREMITES DU FOND DE FISSURE
!
    call jeveuo(info, 'L', jinfo)
    ptextr = '&&XORIFF.PTEXTR'
    call xffext(jinfo, nfon, nmafon, listpt, ptextr,&
                nbptex)
    typfon = ' '
    if (zk16(jinfo-1+3) .eq. 'FERME') typfon = 'FERME'
!
    call jeveuo(listpt, 'L', jlistp)
    call jeveuo(ptextr, 'L', jptext)
    call jeveuo(fonmul, 'L', jfonmu)
!
!     VECTEUR DES INDICES DES POINTS DU FOND ORDONNES
    tabpt = '&&XORIFF.TABPT'
    call wkvect(tabpt, 'V V I', nfon, jtabpt)
!
!     INDICE DU PREMIER POINT
    indipt = zi(jptext-1+1)
    zi(jtabpt-1+1) = indipt
    zi(jptext-1+1) = 0
    zi(jfonmu-1+1) = 1
    zr(jfono-1+11*(1-1)+4) = 0.d0
!
    nbfond = 1
!
    do 10 ipt = 1, nfon-1
        do 11 ima = 1, nmafon
!
            if (zi(jlistp-1+2*(ima-1)+2) .eq. 0) goto 11
!
!---      LE PREMIER INDICE CORRESPOND A CELUI RECHERCHE
            if (zi(jlistp-1+2*(ima-1)+1) .eq. zi(jtabpt-1+ipt)) then
                if (ipt .gt. 2) then
                    if (zi(jtabpt-1+ipt-2) .eq. zi(jlistp-1+2*(ima-1)+2)) then
!               DOUBLON DANS LA LISTE
                        zi(jlistp-1+2*(ima-1)+2) = 0
                        goto 11
                    endif
                endif
!
                zi(jtabpt-1+ipt+1) = zi(jlistp-1+2*(ima-1)+2)
                zi(jlistp-1+2*(ima-1)+2) = 0
!
!           CALCUL DE L'ABSCISSE CURVILIGNE
                indipt = zi(jtabpt-1+ipt+1)
                indicm = zi(jtabpt-1+ipt)
                do 700 k = 1, 3
                    p(k)=zr(jfono-1+11*(indipt-1)+k)
                    m(k)=zr(jfono-1+11*(indicm-1)+k)
700              continue
                absc = zr(jfono-1+11*(indicm-1)+4)
                zr(jfono-1+11*(indipt-1)+4) = absc + padist(3,m,p)
!
!---      LE DEUXIEME INDICE CORRESPOND A CELUI RECHERCHE
                elseif (zi(jlistp-1+2*(ima-1)+2).eq.zi(jtabpt-1+ipt))&
            then
                if (ipt .gt. 2) then
                    if (zi(jtabpt-1+ipt-2) .eq. zi(jlistp-1+2*(ima-1)+1)) then
!               DOUBLON DANS LA LISTE
                        zi(jlistp-1+2*(ima-1)+2) = 0
                        goto 11
                    endif
                endif
!
                zi(jtabpt-1+ipt+1) = zi(jlistp-1+2*(ima-1)+1)
                zi(jlistp-1+2*(ima-1)+2) = 0
!
!           CALCUL DE L'ABSCISSE CURVILIGNE
                indipt = zi(jtabpt-1+ipt+1)
                indicm = zi(jtabpt-1+ipt)
                do 800 k = 1, 3
                    p(k)=zr(jfono-1+11*(indipt-1)+k)
                    m(k)=zr(jfono-1+11*(indicm-1)+k)
800              continue
                absc = zr(jfono-1+11*(indicm-1)+4)
                zr(jfono-1+11*(indipt-1)+4) = absc + padist(3,m,p)
!
            endif
11      continue
!
!       ON N'A PAS TROUVE DE POINT A ASSOCIER A IPT: C'EST UN POINT
!       EXTREMITE ( CAS DES FONDS MULTIPLES )
        if (zi(jtabpt-1+ipt+1) .eq. 0) then
!
!         PRESENCE DE PLUSIEURS FONDS FERMES INTERDIT
            if (typfon .eq. 'FERME') call u2mess('F', 'XFEM_20')
!
            indice = 0
!
!         VERIFICATION QUE LE DERNIER POINT EST UNE EXTREMITE DU FOND
            do 12 iptext = 1, nbptex
                if (zi(jptext-1+iptext) .eq. zi(jtabpt-1+ipt)) then
                    zi(jfonmu-1+2*(nbfond-1)+2) = ipt
                    indice = 1
                    goto 13
                endif
12          continue
13          continue
!
            call assert(indice.ne.0)
            zi(jptext-1+iptext) = 0
!
!         RECHERCHE D'UN NOUVEAU POINT D'EXTREMITE POUR DEBUTER LE
!         NOUVEAU FOND DE FISSURE
            do 14 iptext = 1, nbptex
                if (zi(jptext-1+iptext) .ne. 0) then
!
                    indipt = zi(jptext-1+iptext)
                    zi(jtabpt-1+ipt+1) = indipt
!
                    zr(jfono-1+11*(indipt-1)+4) = 0.d0
                    zi(jptext-1+iptext) = 0
                    nbfond = nbfond + 1
                    zi(jfonmu-1+2*(nbfond-1)+1) = ipt+1
!
                    goto 15
                endif
14          continue
!
!         PRESENCE DE FONDS OUVERTS ET DE FONDS FERMES INTERDIT
            call u2mess('F', 'XFEM_21')
!
        endif
15      continue
10  continue
!
    zi(jfonmu-1+2*(nbfond-1)+2) = nfon
!
    call u2mesi('I', 'XFEM_34', 1, nbfond)
!
!     ORDONNANCEMENT DE FONDFISS, DE BASEFOND ET DE FOND.TAILLE_R
    call xffcr(nfon, jfono, jbaso, jtailo, jtabpt,&
               typfon, jfon, jbas, jtail)
!
    if (goinop) then
        call jedetr(ptextr)
        call jedetr(tabpt)
    endif
    call jedema()
end subroutine
