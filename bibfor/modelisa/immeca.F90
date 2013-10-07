subroutine immeca(tablca, lirela, mailla, nbnobe, nunobe,&
                  icabl, nbnoca, xnoca, ynoca, znoca,&
                  ncncin, nmabet)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : IMMERSION DES NOEUDS D'UN CABLE DANS LE MAILLAGE BETON
!  -----------   ET DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES
!                DDLS DES NOEUDS DU CABLE ET LES DDLS DES NOEUDS VOISINS
!                DE LA STRUCTURE BETON
!                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!                EN SORTIE ON AJOUTE DES LIGNES DANS LA TABLE RESULTAT
!                LES CASES RENSEIGNEES CORRESPONDENT AUX PARAMETRES
!                <MAILLE_BETON_VOISINE>, <NOEUD_BETON_VOISIN> ET
!                <INDICE_IMMERSION>
!                LA SD DE TYPE LISTE_DE_RELATIONS EST MISE A JOUR
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : LIRELA : CHARACTER*19 , SCALAIRE
!                    NOM DE LA SD DE TYPE LISTE_DE_RELATIONS
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : NBNOBE : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS APPARTENANT A LA STRUCTURE BETON
!  IN     : NUNOBE : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES NOEUDS APPARTENANT A LA STRUCTURE BETON
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : XNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ABSCISSES X DES NOEUDS APPARTENANT AUX CABLES
!  IN     : YNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ORDONNEES Y DES NOEUDS APPARTENANT AUX CABLES
!  IN     : ZNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    COTES Z DES NOEUDS APPARTENANT AUX CABLES
!  IN     : NCNCIN : CHARACTER*24 ,
!                    OBJET CONNECTIVITE INVERSE POUR LES MAILLES BETON
!  IN     : NMABET : CHARACTER*24 ,
!                    OBJET CONTENANT LES MAILLES BETON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/immeno.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/reci3d.h"
#include "asterfort/tbajli.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mailla
    character(len=19) :: lirela, nunobe, xnoca, ynoca, znoca, tablca
    integer :: nbnobe, icabl, nbnoca(*)
    character(len=24) :: ncncin, nmabet
    character(len=24) :: valk(2)
!
! VARIABLES LOCALES
! -----------------
    integer :: nselec
    parameter     (nselec=5)
    integer :: ideca, immer, inob1, inob2, inobe, inoca, ipara, itetra, jcoor
    integer :: jcxma, jd2, jnoca, jnod2, jnunob, jtblp, jtbnp, jxca, jxyzma
    integer :: jyca, jzca, nbcnx, nblign, nbno, nbpara, nnomax, noe
    integer :: noebe(nselec), numail, nbval, nbval2, iret, ibid, noebec
    real(kind=8) :: d2, d2min(nselec), dx, dy, dz, rbid, x3dca(3)
    real(kind=8) :: x3dca2(3), axe(3), xnorm, xnorm2, zero, xbar(4)
    real(kind=8) :: rayon
    real(kind=8) :: long, longcy, longca, d2minc
    integer :: ifm, niv
    character(len=8) :: nnoec2, k8b, presen(2), k8vide, noancr(2)
    complex(kind=8) :: cbid
    character(len=3) :: k3b
    character(len=8) :: nnoeca, voisin(2)
    character(len=24) :: coorno, nomama, nonoca, nonoma, nogrna(2)
    integer :: n1, ibe, jbe
!
    character(len=24) :: param(3), parcr
    integer :: iarg
    data          param /'MAILLE_BETON_VOISINE    ',&
     &                     'NOEUD_BETON_VOISIN      ',&
     &                     'INDICE_IMMERSION        '/
    data          parcr /'NOEUD_CABLE             '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call infmaj()
    call jemarq()
    call infniv(ifm, niv)
!
    cbid=(0.d0,0.d0)
    rbid=0.d0
    zero = 0.0d0
    longcy = zero
    longca = zero
    k8vide = '        '
!
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX DONNEES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 1.1 OBJETS DU MAILLAGE
! ---
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    nomama = mailla//'.NOMMAI'
    nonoma = mailla//'.NOMNOE'
!
!
! RECUPERATION DES MOTS-CLES
!
!     TRAITEMENT DU MOT-CLE 'CONE'
    call getvr8('CONE', 'RAYON', iocc=1, scal=rayon, nbret=nbval)
    call getvr8('CONE', 'LONGUEUR', iocc=1, scal=long, nbret=nbval2)
    if (nbval .eq. 0) then
        rayon = zero
    endif
    if (nbval2 .eq. 0) then
        long = zero
    endif
    presen(1) = k8vide
    presen(2) = k8vide
    call getvtx('CONE', 'PRESENT', iocc=1, nbval=2, vect=presen,&
                nbret=n1)
!
!
!     TRAITEMENT DU MOT-CLE 'NOEUD_ANCRAGE'
    noancr(1) = k8vide
    noancr(2) = k8vide
    call getvtx('DEFI_CABLE', 'NOEUD_ANCRAGE', iocc=icabl, nbval=2, vect=noancr,&
                nbret=n1)
!
!     TRAITEMENT DU MOT-CLE 'GROUP_NO_ANCRAGE'
    if (n1 .eq. 0) then
        call getvem(mailla, 'GROUP_NO', 'DEFI_CABLE', 'GROUP_NO_ANCRAGE', icabl,&
                    iarg, 2, nogrna(1), ibid)
!
        call utnono(' ', mailla, 'NOEUD', nogrna(1), k8b,&
                    iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nogrna(1))
        else if (iret.eq.1) then
            valk(1) = nogrna(1)
            valk(2) = k8b
            call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
        endif
        noancr(1) = k8b
!
        call utnono(' ', mailla, 'NOEUD', nogrna(2), k8b,&
                    iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nogrna(2))
        else if (iret.eq.1) then
            valk(1) = nogrna(2)
            valk(2) = k8b
            call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
        endif
        noancr(2) = k8b
    endif
!
!
! 1.2 DONNEES RELATIVES AU CABLE
! ---
!.... NOMBRE DE NOEUDS
!
    nbno = nbnoca(icabl)
!
!.... NOMS DES NOEUDS
!
    call jeveuo(tablca//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp)
    nblign = zi(jtbnp+1)
    ideca = nblign - nbno
    call jeveuo(tablca//'.TBLP', 'L', jtblp)
    do 10 ipara = 1, nbpara
        if (zk24(jtblp+4*(ipara-1)) .eq. parcr) then
            nonoca = zk24(jtblp+4*(ipara-1)+2)
            call jeveuo(nonoca, 'L', jnoca)
            goto 11
        endif
10  end do
11  continue
!
!.... COORDONNEES DES NOEUDS
!
    call jeveuo(xnoca, 'L', jxca)
    call jeveuo(ynoca, 'L', jyca)
    call jeveuo(znoca, 'L', jzca)
!
! 1.3 NUMEROS DES NOEUDS DE LA STRUCTURE BETON
! ---
    call jeveuo(nunobe, 'L', jnunob)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   IMMERSION DES NOEUDS DU CABLE DANS LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 2.1 CREATION D'OBJETS DE TRAVAIL
! ---
!.... LES MAILLES APPARTENANT A LA STRUCTURE BETON SONT DES MAILLES
!.... TETRA4, TETRA10, PYRAM5, PYRAM13, PENTA6, PENTA15,
!.... HEXA8, HEXA20 OU HEXA27
!.... LA VERIFICATION A ETE EFFECTUEE EN AMONT PAR LA ROUTINE TOMABE
!.... LE NOMBRE DE NOEUDS MAXIMAL SUR UNE MAILLE VAUT DONC 27
!
    nnomax = 27
    call wkvect('&&IMMECA.XYZ_NOEMAI', 'V V R', 3*nnomax, jxyzma)
    call wkvect('&&IMMECA.CNX_MAILLE', 'V V I', nnomax, jcxma)
!
    call wkvect('&&IMMECA.D2_MIN_MAX', 'V V R', nbnobe, jd2)
    call wkvect('&&IMMECA.NO_MIN_MAX', 'V V I', nbnobe, jnod2)
!
!.... CALCUL DE LA LONGUEUR TOTALE DU CABLE
!
    do 101 inoca = 1, (nbno-1)
!
        nnoeca = zk8(jnoca+ideca+inoca-1)
!
!       DANS LE CAS OU LE NOEUD INITIAL CORRESPOND AU NOEUD FINAL DONNE
!       PAR L'UTILISATEUR ON DOIT INVERSER LES DIRECTIVES 'PRESENT'
        if (inoca .eq. 1) then
            if (nnoeca .eq. noancr(2)) then
                k8b = presen(2)
                presen(2) = presen(1)
                presen(1) = k8b
            endif
        endif
!
        x3dca(1) = zr(jxca+ideca+inoca-1)
        x3dca(2) = zr(jyca+ideca+inoca-1)
        x3dca(3) = zr(jzca+ideca+inoca-1)
!
        nnoec2 = zk8(jnoca+ideca+inoca-1+1)
        x3dca2(1) = zr(jxca+ideca+inoca-1+1)
        x3dca2(2) = zr(jyca+ideca+inoca-1+1)
        x3dca2(3) = zr(jzca+ideca+inoca-1+1)
!
        axe(1) = (x3dca2(1) - x3dca(1))
        axe(2) = (x3dca2(2) - x3dca(2))
        axe(3) = (x3dca2(3) - x3dca(3))
        xnorm2 = axe(1)*axe(1) + axe(2)*axe(2) + axe(3)*axe(3)
!
        if (xnorm2 .eq. zero) then
            call utmess('F', 'MODELISA4_70')
        endif
!
        xnorm = sqrt(xnorm2)
        longca = longca + xnorm
!
101  end do
!
    if (niv .eq. 2) then
        write(ifm,*) '------------------------------------------'
        write(ifm,*) ' DEFINITION DES RELATIONS CINEMATIQUES'
        if (rayon .eq. zero) then
            write(ifm,*) '  CONE : PAS DE CONE'
        else
            write(ifm,*) '  RAYON DU CONE : ',rayon
            write(ifm,*) '  LONGUEUR DU CONE : ',long
        endif
        write(ifm,*) '  LONGUEUR DU CABLE : ',longca
        write(ifm,*) ' '
    endif
!
!
!
! 2.2 BOUCLE SUR LE NOMBRE DE NOEUDS DU CABLE
! ---
    do 100 inoca = 1, nbno
!
!
        nnoeca = zk8(jnoca+ideca+inoca-1)
        x3dca(1) = zr(jxca+ideca+inoca-1)
        x3dca(2) = zr(jyca+ideca+inoca-1)
        x3dca(3) = zr(jzca+ideca+inoca-1)
!
        nnoec2 = zk8(jnoca+ideca+inoca-1+1)
        x3dca2(1) = zr(jxca+ideca+inoca-1+1)
        x3dca2(2) = zr(jyca+ideca+inoca-1+1)
        x3dca2(3) = zr(jzca+ideca+inoca-1+1)
!
        if (niv .eq. 2) then
            write(ifm,*) ' '
            write(ifm,*) ' '
            if (inoca .lt. nbno) then
                write(ifm,*) 'NOEUDS CABLE : ',nnoeca,' - ',nnoec2
            else
                write(ifm,*) 'NOEUD CABLE : ',nnoeca
            endif
        endif
!
!
! 2.2.0  CREATION DU VECTEUR AXE, RELIANT DEUX NOEUDS CABLES CONSECUTIFS
! .....  POUR LE CALCUL DES DISTANCES AU CYLINDRE
!
        if (inoca .ne. nbno) then
            axe(1) = (x3dca2(1) - x3dca(1))
            axe(2) = (x3dca2(2) - x3dca(2))
            axe(3) = (x3dca2(3) - x3dca(3))
            xnorm2 = axe(1)*axe(1) + axe(2)*axe(2) + axe(3)*axe(3)
            xnorm = sqrt(xnorm2)
        else
            xnorm = 0.d0
        endif
!
! ... CHOIX DU TRAITEMENT :
!
!  SI LA LONGUEUR (OU LE RAYON) DU CONE EST NULLE (PAS DE CONE)
        if ((long.eq.zero) .or. (rayon.eq.zero)) goto 112
!
!  TESTE SI ON EST EN DEHORS DES ZONES DE DEFINITIONS DES TUNNELS
        if ((longcy.gt.long) .and. (longcy.lt.(longca-long))) goto 112
!
!  SINON TESTE SI ON A DEMANDE DES TUNNELS
!
        if ((longcy.lt.long) .and. (presen(1)(1:3).eq.'NON')) goto 112
!
        if ((longcy.gt.(longca-long)) .and. (presen(2)(1:3).eq.'NON')) goto 112
!
!  SINON ON DEFINI LE CONE
!
!     --------------------------------------------------------
!     CAS 1 : ON DEFINIT LE CONE POUR ATTACHER LES NOEUDS
!     --------------------------------------------------------
!      Note : on ne fait rien au niveau du fortran
!      (voir la macrocommande DEFI_CABLE_BP)
!
!
        if (niv .eq. 2) then
            write(ifm,*) '-> ON DEFINIT LE CYLINDRE D''AXE ', nnoeca,&
            ' - ',nnoec2
        endif
!
        longcy = longcy + xnorm
        goto 100
!
!
!     ---------------------------------------------------------
!     CAS 2 : ON ATTACHE UN SEUL NOEUD DU BETON AU NOEUD CABLE
!     ---------------------------------------------------------
!
112      continue
        if (niv .eq. 2) then
            write(ifm,*) '-> ON ATTACHE LE NOEUD OU LA MAILLE BETON '//&
     &             'LA PLUS PROCHE'
        endif
!
        longcy = longcy + xnorm
!
!
! 2.2.1  DETERMINATION DU NOEUD DE LA STRUCTURE BETON LE PLUS PROCHE
! .....  DU NOEUD CABLE COURANT
!
! ON DETERMINE LES NSELEC NOEUDS LES PLUS PROCHES
!
        do 114 ibe = 1, nselec
            d2min(ibe) = r8maem()
            noebe(ibe) = 0
114      continue
!
        noebec=0
        do 110 inobe = 1, nbnobe
            noe = zi(jnunob+inobe-1)
            dx = x3dca(1) - zr(jcoor+3*(noe-1) )
            dy = x3dca(2) - zr(jcoor+3*(noe-1)+1)
            dz = x3dca(3) - zr(jcoor+3*(noe-1)+2)
            d2 = dx * dx + dy * dy + dz * dz
            do 111 ibe = 1, nselec
                if (d2 .lt. d2min(ibe)) then
                    do 122 jbe = 0, nselec-ibe-1
                        d2min(nselec-jbe)=d2min(nselec-jbe-1)
                        noebe(nselec-jbe)=noebe(nselec-jbe-1)
122                  continue
                    d2min(ibe)=d2
                    noebe(ibe)=noe
                    goto 113
                endif
111          continue
113          continue
            zr(jd2+inobe-1) = d2
            zi(jnod2+inobe-1) = noe
110      continue
!
        if (niv .eq. 2) then
            write(ifm,*) '   INFOS : DISTANCE MINIMALE : ',sqrt(d2min(1))
        endif
!
!
! 2.2.2  TENTATIVE D'IMMERSION DU NOEUD CABLE DANS LES MAILLES
! .....  AUXQUELLES APPARTIENT LE NOEUD BETON LE PLUS PROCHE
!
        do 115 ibe = 1, nselec
!          ATTENTION IL PEUT Y AVOIR MOINS QUE NSELEC NOEUDS
!          DE BETON
            if (noebe(ibe) .eq. 0) goto 116
!
            call immeno(ncncin, nmabet, mailla, x3dca(1), noebe(ibe),&
                        numail, nbcnx, zi(jcxma), zr(jxyzma), itetra,&
                        xbar(1), immer)
            if (immer .ge. 0) then
                noebec = noebe(ibe)
                goto 116
            endif
115      continue
116      continue
!
! 2.2.3  EN CAS D'ECHEC DE LA TENTATIVE PRECEDENTE
! .....
        if (immer .lt. 0) then
!
!.......... ON CREE UNE LISTE ORDONNEE DES NOEUDS DE LA STRUCTURE BETON
!.......... DU PLUS PROCHE AU PLUS ELOIGNE DU NOEUD CABLE CONSIDERE
!
            do 120 inob1 = 1, nbnobe-1
                d2minc = zr(jd2+inob1-1)
                noebec = zi(jnod2+inob1-1)
                inobe = inob1
                do 121 inob2 = inob1+1, nbnobe
                    if (zr(jd2+inob2-1) .lt. d2minc) then
                        d2minc = zr(jd2+inob2-1)
                        noebec = zi(jnod2+inob2-1)
                        inobe = inob2
                    endif
121              continue
                if (inobe .gt. inob1) then
                    d2 = zr(jd2+inob1-1)
                    noe = zi(jnod2+inob1-1)
                    zr(jd2+inob1-1) = d2minc
                    zi(jnod2+inob1-1) = noebec
                    zr(jd2+inobe-1) = d2
                    zi(jnod2+inobe-1) = noe
                endif
120          continue
!
            if (niv .eq. 2) then
                write(ifm,*) '   INFOS : DISTANCE MINIMALE : ',sqrt(d2)
            endif
!
!
!.......... LA TENTATIVE D'IMMERSION DANS LES MAILLES AUXQUELLES
!.......... APPARTIENT LE NOEUD BETON LE PLUS PROCHE A DEJA ETE
!.......... EFFECTUEE, SANS SUCCES
!.......... ON EFFECTUE DE NOUVELLES TENTATIVES EN UTILISANT LES NOEUDS
!.......... DE LA LISTE ORDONNEE PRECEDENTE, DU SECOND JUSQU'AU DERNIER
!.......... REPETER
            do 130 inobe = nselec, nbnobe
                noebec = zi(jnod2+inobe-1)
!............. TENTATIVE D'IMMERSION DU NOEUD CABLE DANS LES MAILLES
!............. AUXQUELLES APPARTIENT LE NOEUD BETON COURANT
                call immeno(ncncin, nmabet, mailla, x3dca(1), noebec,&
                            numail, nbcnx, zi(jcxma), zr(jxyzma), itetra,&
                            xbar(1), immer)
!............. SORTIE DU BLOC REPETER EN CAS DE SUCCES
                if (immer .ge. 0) goto 131
130          continue
131          continue
!
        endif
!
!
!
! 2.2.4  SORTIE EN ERREUR FATALE SI ECHEC PERSISTANT
! .....
        if (immer .lt. 0) then
            write(k3b,'(I3)') icabl
            valk(1) = k3b
            valk(2) = nnoeca
            call utmess('F', 'MODELISA4_71', nk=2, valk=valk)
        endif
!
! 2.2.5  DETERMINATION DES RELATIONS CINEMATIQUES
! .....
        call reci3d(lirela, mailla, nnoeca, noebec, nbcnx,&
                    zi(jcxma), itetra, xbar(1), immer)
!
! 2.2.6  MISE A JOUR DE LA SD TABLE
! .....
        call jenuno(jexnum(nomama, numail), voisin(1))
        ASSERT(noebec.ne.0)
        call jenuno(jexnum(nonoma, noebec), voisin(2))
        call tbajli(tablca, 3, param, [immer], [rbid],&
                    [cbid], voisin(1), ideca+ inoca)
!
100  end do
!
!  FIN BOUCLE SUR NBNO
!
!
! --- MENAGE
!
    call jedetr('&&IMMECA.XYZ_NOEMAI')
    call jedetr('&&IMMECA.CNX_MAILLE')
    call jedetr('&&IMMECA.D2_MIN_MAX')
    call jedetr('&&IMMECA.NO_MIN_MAX')
!
    call jedema()
!
! --- FIN DE IMMECA.
end subroutine
