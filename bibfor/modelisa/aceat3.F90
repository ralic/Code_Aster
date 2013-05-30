subroutine aceat3(noma, nomu, nbtuy, nbpart, nbmap,&
                  elpar, nopar, ivr, ifm, nbzk,&
                  nozk, cozk, isens, coor, epsi,&
                  crit, nno, nmmt)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/angco4.h'
    include 'asterfort/angcou.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/normev.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: noma, nomu, crit
    integer :: nbpart, nbtuy, nbmap(nbpart), elpar(nbpart, nbtuy), ivr(3)
    integer :: nopar(nbpart, nno, nbtuy), nbzk, nozk(nbzk), isens(nbpart), ifm
    integer :: nmmt(*), nno, icmp, iavant, no4, nbcmp, icoud2
    real(kind=8) :: cozk(3*nbzk), coor(*), coor3(12), zk1(3), zk2(3), zk3(3)
    real(kind=8) :: angl1(3), angl2(3), angl3(3), epsi, angl4(3)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES TUYAUX DANS UNE CARTE
! ----------------------------------------------------------------------
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMU   : NOM DU CONCEPT RESULTAT
! IN  NBTUY  : NOMBRE D'ELEMENTS TUYAU
! IN  NBPAT  : NOMBRE DE PARTIES CONNEXES DE TUYAUX
! IN  NBMAP  : NOMBRE DE MAILLES PAR PARTIE
! IN  ELPAR  : NUMERO DES MAILLES DE CHAQUE PARTIE DANS L'ORDRE
! IN  NOPAR  : NUMERO DES NOEUDS  DE CHAQUE PARTIE DANS L'ORDRE
! IN  IVR    : (3) = INFO 2
! IN  IFM    : FICHIER MESSAGES
! IN  NBZK   : NOMBRE D'OCCURENCES DE GENE_TUYAU
! IN  NOZK   : NUMEROS DES NOEUDS OU ZK EST DONNE
! IN  COZK   : COORDONNES DES ZK
! IN  ISENS  : SENS DE PARCOURS DES MAILLES
! IN  COOR   : COORDONNES DES NOEUDS
! IN  NMMT   : INDIQUE SI MODI_METRIQUE POUR CHAQUE MAILLE
! ----------------------------------------------------------------------
    character(len=8) :: nommai, nomno1, nomno2, nomno3, nomno4
    character(len=19) :: cartor, mlgnma, mlgnno
    character(len=24) :: tmpnor, tmpvor
    integer :: jdcmpo, jdvlvo, izk, iok1, iok2, ipa, imfin, i, ima, nummai
    integer :: no1, no2, no3, icoude, im0, nbdroi, nbcoud
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), norme, pgl4(3, 3)
!
!-----------------------------------------------------------------------
    real(kind=8) :: dn1n2, omega, rayon, theta, vx, vy, vz
!
!-----------------------------------------------------------------------
    call jemarq()
!
!     VERIFICATION QUE LES NOEUDS DONNES SONT DES EXTREMITES
! --- AFFECTATION DES VALEURS DU TAMPON DANS LA CARTE ORIENTATION :
!     -------------------------------------------------------------
    mlgnma = noma//'.NOMMAI'
    mlgnno = noma//'.NOMNOE'
    cartor = nomu//'.CARORIEN'
    tmpnor = cartor//'.NCMP'
    tmpvor = cartor//'.VALV'
    call jeveuo(tmpnor, 'E', jdcmpo)
    call jeveuo(tmpvor, 'E', jdvlvo)
    zk8(jdcmpo) = 'ALPHA'
    zk8(jdcmpo+ 1) ='BETA'
    zk8(jdcmpo+ 2) ='GAMMA'
    zk8(jdcmpo+ 3) ='ALPHA2'
    zk8(jdcmpo+ 4) ='BETA2'
    zk8(jdcmpo+ 5) ='GAMMA2'
    zk8(jdcmpo+ 6) ='ALPHA3'
    zk8(jdcmpo+ 7) ='BETA3'
    zk8(jdcmpo+ 8) ='GAMMA3'
    icmp=8
    if (nno .eq. 4) then
        zk8(jdcmpo+ 9) ='ALPHA4'
        zk8(jdcmpo+10) ='BETA4'
        zk8(jdcmpo+11) ='GAMMA4'
        icmp=11
    endif
    zk8(jdcmpo+icmp+1) ='ICOUDE'
    zk8(jdcmpo+icmp+2) ='DN1N2'
    zk8(jdcmpo+icmp+3) ='RCOURB'
    zk8(jdcmpo+icmp+4) ='ANGCOU'
    zk8(jdcmpo+icmp+5) ='ANGZZK'
!
!     LES NOEUDS ASSOCIES A GENE_TUYAUX DOIVENT ETRE UNE
!     DES EXTREMITES
!
    do 62 ipa = 1, nbpart
        isens(ipa)=0
62  end do
    do 60 izk = 1, nbzk
        iok1=0
        iok2=0
        do 61 ipa = 1, nbpart
            imfin=nbmap(ipa)
            if (nozk(izk) .eq. (nopar(ipa,1,1))) then
                iok1=1
                if (isens(ipa) .eq. 0) then
                    isens(ipa)=izk
                else
                    call u2mess('F', 'MODELISA_24')
                endif
            endif
            if (nozk(izk) .eq. (nopar(ipa,2,imfin))) then
                iok2=1
                if (isens(ipa) .eq. 0) then
                    isens(ipa)=-izk
                else
                    call u2mess('F', 'MODELISA_24')
                endif
            endif
61      continue
        if ((iok1+iok2) .ne. 1) then
            call u2mess('F', 'MODELISA_25')
        endif
60  end do
!
!     STOCKAGE DANS LA CARTE DES PARAMETRES GEOMETRIQUES CALCULES
!     PAR ANGCOU ET DE LA GENERATRICE CONTINUE SUR LES TUYAUX
!
    nbdroi=0
    nbcoud=0
!
    do 68 ipa = 1, nbpart
        if (ivr(3) .eq. 1) write(ifm,1000) ipa,nbmap(ipa)
        izk=isens(ipa)
        if (izk .eq. 0) then
!
!        PAS DE VECTEUR FOURNI : ON EN CREE UN
!
            no1=nopar(ipa,1,1)
            no2=nopar(ipa,2,1)
            vx = coor(3*no2-3+1)-coor(3*no1-3+1)
            vy = coor(3*no2-3+2)-coor(3*no1-3+2)
            vz = coor(3*no2-3+3)-coor(3*no1-3+3)
            zk1(1)=-vy
            zk1(2)=vx
            zk1(3)=0.d0
            call normev(zk1, norme)
            if (norme .lt. 1.d-4) then
                zk1(1)=0.d0
                zk1(2)=-vz
                zk1(3)=vy
                call normev(zk1, norme)
            endif
        else
            do 64 i = 1, 3
                zk1(i)=cozk(3*(abs(izk)-1)+i)
64          continue
            call normev(zk1, norme)
        endif
        iavant=-1
        do 67 im0 = 1, nbmap(ipa)
            if (izk .ge. 0) then
                ima=im0
            else
                ima=nbmap(ipa)-im0+1
            endif
            no1=nopar(ipa,1,ima)
            no2=nopar(ipa,2,ima)
            no3=nopar(ipa,3,ima)
            if (nno .eq. 4) then
                no4=nopar(ipa,4,ima)
            endif
            nummai=elpar(ipa,ima)
            do 65 i = 1, 3
                coor3(i )=coor(3*no1-3+i)
                coor3(3+i)=coor(3*no2-3+i)
                coor3(6+i)=coor(3*no3-3+i)
                if (nno .eq. 4) then
                    coor3(9+i)=coor(3*no4-3+i)
                endif
65          continue
            if (nno .eq. 3) then
                call angcou(coor3, zk1, izk, icoude, zk2,&
                            rayon, theta, angl1, angl2, angl3,&
                            pgl1, pgl2, pgl3, omega, dn1n2,&
                            epsi, crit, zk3)
                do 631 i = 1, 3
                    angl4(i)=0.d0
631              continue
            else if (nno.eq.4) then
                call angco4(coor3, zk1, izk, icoude, zk2,&
                            rayon, theta, angl1, angl2, angl3,&
                            angl4, pgl1, pgl2, pgl3, pgl4,&
                            omega, dn1n2, epsi, crit)
            endif
            do 63 i = 1, 3
                zr(jdvlvo-1+ i)= angl1(i)
                zr(jdvlvo-1+3+i)= angl2(i)
                zr(jdvlvo-1+6+i)= angl3(i)
63          continue
            icmp=9
            nbcmp=14
            if (nno .eq. 4) then
                do 74 i = 1, 3
                    zr(jdvlvo-1+9+i)= angl4(i)
74              continue
                icmp=12
                nbcmp=17
            endif
!
            if (icoude .eq. 0) then
                nbdroi=nbdroi+1
            else
                nbcoud=nbcoud+1
            endif
!
!              MODI_METRIQUE
!
            if (nmmt(nummai) .eq. 0) then
                icoud2=icoude+10
            else if (nmmt(nummai).eq.1) then
                icoud2=icoude
            else
                call jenuno(jexnum(mlgnma, nummai), nommai)
                call u2mesk('F', 'MODELISA_26', 1, nommai)
            endif
!
            zr(jdvlvo-1+icmp+1) = icoud2
            zr(jdvlvo-1+icmp+2) = dn1n2
            zr(jdvlvo-1+icmp+3) = rayon
            zr(jdvlvo-1+icmp+4) = theta
            zr(jdvlvo-1+icmp+5) = omega
!
            call nocart(cartor, 3, ' ', 'NUM', 1,&
                        ' ', nummai, ' ', nbcmp)
!
            if (ivr(3) .eq. 1) then
                call jenuno(jexnum(mlgnma, nummai), nommai)
                call jenuno(jexnum(mlgnno, no1), nomno1)
                call jenuno(jexnum(mlgnno, no2), nomno2)
                call jenuno(jexnum(mlgnno, no3), nomno3)
                if (nno .eq. 4) then
                    call jenuno(jexnum(mlgnno, no4), nomno4)
                else
                    nomno4=' '
                endif
                if (icoude .ne. iavant) then
                    if (nno .eq. 3) then
                        if (icoude .eq. 0) then
                            write(ifm,1031)
                        else
                            write(ifm,1032)
                        endif
                    else if (nno.eq.4) then
                        if (icoude .eq. 0) then
                            write(ifm,1031)
                        else
                            write(ifm,1032)
                        endif
                    endif
                    iavant=icoude
                endif
                if (izk .ge. 0) then
                    if (icoude .eq. 0) then
                        write(ifm,1010) nommai,nomno1,nomno2,nomno3,&
                        nomno4, (zk1(i),i=1,3),(angl1(i),i=1,3)
                    else
                        write(ifm,1011) nommai,nomno1,nomno2,nomno3,&
                        nomno4, (zk1(i),i=1,3),(zk2(i),i=1,3),rayon,&
                        theta,omega ,(angl1(i),i=1,3),(angl2(i),i=1,3)&
                        ,(angl3(i),i=1,3)
                    endif
                else
                    if (icoude .eq. 0) then
                        write(ifm,1010) nommai,nomno1,nomno2,nomno3,&
                        nomno4, (zk1(i),i=1,3),(angl1(i),i=1,3)
                    else
                        write(ifm,1011) nommai,nomno1,nomno2,nomno3,&
                        nomno4, (zk2(i),i=1,3),(zk1(i),i=1,3),rayon,&
                        theta,omega ,(angl1(i),i=1,3),(angl2(i),i=1,3)&
                        ,(angl3(i),i=1,3)
                    endif
                endif
            endif
            do 69 i = 1, 3
                zk1(i)=zk2(i)
69          continue
67      continue
68  end do
!
    write(ifm,1055) nbdroi
    write(ifm,1056) nbcoud
!
    1031 format(&
     &3x,'MAILLE  NOEUD1  NOEUD2  NOEUD3  NOEUD4   TYPE   ',&
     &'Z1_X',8x,'Z1_Y',8x,'Z1_Z',8x,'ALPHA1',6x,'BETA1',7x,'GAMMA1')
    1032 format(&
     &3x,'MAILLE  NOEUD1  NOEUD2  NOEUD3  NOEUD4   TYPE   ',&
     &'Z1_X',8x,'Z1_Y',8x,'Z1_Z',8x,'Z2_X',8x,'Z2_Y',8x,'Z2_Z',&
     &8x,'RAYON',7x,'ANGLE',7x,'OMEGA',&
     &7x,'ALPHA1',6x,'BETA1',7x,'GAMMA1',&
     &6x,'ALPHA2',6x,'BETA2',7x,'GAMMA2',&
     &6x,'ALPHA3',6x,'BETA3',7x,'GAMMA3')
!
    1000 format(3x,'TUYAUTERIE NUMERO : ',i6,' NOMBRE DE MAILLES : ',i6)
!
    1010 format(3x,5a8,1x,'DROIT',1x,9(d11.4,1x))
    1011 format(3x,5a8,1x,'COUDE',1x,18(d11.4,1x))
!
    1055 format(3x,'NOMBRE TOTAL D ELEMENTS TUYAU DROITS ',1x,i6)
    1056 format(3x,'NOMBRE TOTAL D ELEMENTS TUYAU COUDES ',1x,i6)
!
    call jedetr(tmpnor)
    call jedetr(tmpvor)
!
    call jedema()
end subroutine
