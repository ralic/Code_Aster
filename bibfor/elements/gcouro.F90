subroutine gcouro(base, resu, noma, nomno, coorn,&
                  lobj2, trav1, trav2, trav3, dir,&
                  nomnoe, fond, direc, stok4)
    implicit none
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTION REALISEE:
!
!     OPTION COURONNE
!     ---------------
!
! 1.  POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
!
! 2.  PUIS ON CALCULE LA DIRECTION DE THETA SI CELLE-CI N'EST PAS DONNEE
!     PAR APPEL A GDIREC
!
! 3.  ENSUITE ON CALCULE THETA SUR TOUS LES NOEUDS DU MAILLAGE
!
!     ------------------------------------------------------------------
! ENTREE:
!        BASE   : BASE DE CREATION DU CHAMP CHAM_NO
!        RESU   : NOM DU CONCEPT DE TYPE CHAM_NO
!        NOMA   : NOM DU MAILLAGE
!        NOMNO  : NOM DE L'OBJET CONTENANT LES NOEUDS DU MAILLAGE
!        NOMNOE : NOMS DES NOEUDS DU FOND DE FISSURE
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
!        LOBJ2  : NOMBRE DE NOEUDS DE GAMM0
!        FOND   : NOM DE CONCEPT POUR DEFI_FOND_FISS CONTENANT LES
!                 NOEUD DU FOND DE FISSURE
!        DIREC  : SI LA DIRECTION EST DONNEE ALORS DIREC=.TRUE.
!                      ON LA RECUPERE ( DIR )
!                 SINON ON LA CALCULE DIREC=.FALSE.
!                       APPEL A GDIREC
!        TRAV1  : RINF
!        TRAV2  : RSUP
!
! SORTIE:
!        STOK4  : DIRECTION DU CHAMP THETA
!                     LISTE DE CHAMPS_NO THETA
!        TRAV3  : MODULE(THETA)
!     ------------------------------------------------------------------
!
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/chpver.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/gdinor.h'
    include 'asterfort/gdirec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/lcprsn.h'
    include 'asterfort/normev.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    character(len=24) :: obj3, numgam, chamno
    character(len=24) :: trav1, trav2, trav3, objor, objex, dirth
    character(len=24) :: norm, stok4, dire4, coorn, nomno, dire5, indicg, resu
    character(len=8) :: fond, noma, nomnoe(*), k8b, chbid
    character(len=16) :: nomcmd, motfac, k16b
    character(len=1) :: base, k1bid
!
    integer :: lobj2, iadrt1, iadrt2, iadrt3, itheta, jvect
    integer :: in2, iadrco, jmin, ielinf, iadnum, iocc, jnorm
    integer :: num, indic, ierd, iadrtt, nec, ibid
    integer :: iret, numa, ienorm, nbdir, idirth, ideeq
    integer :: irefe, nbnoff, iebas
!
    real(kind=8) :: dirx, diry, dirz, xi1, yi1, zi1, xj1, yj1, zj1
    real(kind=8) :: xij, yij, zij, eps, d, tei, tej
    real(kind=8) :: xm, ym, zm, xim, yim, zim, s, dmin, smin, xn, yn, zn
    real(kind=8) :: rii, rsi, alpha, valx, valy, valz, norm2
    real(kind=8) :: norme, vecx, vecy, vecz, dir(3), tmpv(3), psca
!
    logical :: direc, suiv, milieu
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iaextr, iaorig, idesc, idiri, idirs, ielsup
    integer :: itanex, itanor, j, lndir, nbel
!-----------------------------------------------------------------------
    call jemarq()
!
    call getres(k8b, k16b, nomcmd)
    if (nomcmd .eq. 'CALC_G') then
        motfac='THETA'
        iocc=1
    else
        motfac=' '
        iocc=0
    endif
!
    call jeveuo(coorn, 'L', iadrco)
    call jeveuo(trav1, 'L', iadrt1)
    call jeveuo(trav2, 'L', iadrt2)
    call jeveuo(trav3, 'E', iadrt3)
    eps = 1.d-06
!
! RECUPERATION  DES NUMEROS DE NOEUDS DE GAMM0
!
    numgam = '&&COURON.NUMGAMM0'
    call wkvect(numgam, 'V V I', lobj2, iadnum)
    do 550 j = 1, lobj2
        call jenonu(jexnom(nomno, nomnoe(j)), zi(iadnum+j-1))
550  end do
!
! RECUPERATION DES DIRECTIONS AUX EXTREMITES DE GAMM0 (EN 3D)
!
    objor = fond//'.DTAN_ORIGINE'
    call jeexin(objor, itanor)
    objex = fond//'.DTAN_EXTREMITE'
    call jeexin(objex, itanex)
!
!     RECUPERATION DU NOMBRE DE NOEUD
!
    call jelira(fond//'.FOND.NOEU', 'LONUTI', nbnoff, k8b)
!
!  SI LEVRE_SUP EST DEFINIE DANS LE CONCEPT FOND
!
    obj3 = fond//'.LEVRESUP.MAIL'
    call jeexin(obj3, ielsup)
!
!  SI LEVRE_INF EST DEFINIE DANS LE CONCEPT FOND
!
    obj3 = fond//'.LEVREINF.MAIL'
    call jeexin(obj3, ielinf)
!
!  SI NORMALE EST DEFINIE DANS LE CONCEPT FOND
!
    norm = fond//'.NORMALE        '
    call jeexin(norm, ienorm)
!
    stok4 = '&&COURON.DIREC'
    call wkvect(stok4, 'V V R', 3*lobj2, in2)
!
    dire4 = '&&COURON.LEVRESUP'
    dire5 = '&&COURON.LEVREINF'
!
!  RECUPERATION DIRECTION DU CHAMP THETA
!
!     DANS LE CAS OU LA NORMALE EST DEFINIE DANS DEFI_FOND_FISS/NORMALE,
!     ON AVERTIT L'UTILISATEUR PAR UNE ALARME SI LA DIRECTION N'EST PAS
!     FOURNIE
    if (.not.direc .and. ienorm .ne. 0) then
        call u2mess('A', 'RUPTURE0_91')
    endif
!     ON VERIFIE QUE LA DIRECTION FOURNIE EST ORTHOGONALE A LA NORMALE
!
    if (direc .and. ienorm .ne. 0) then
        call jeveuo(norm, 'L', jnorm)
        call dcopy(3, zr(jnorm), 1, tmpv, 1)
        call normev(dir, norme)
        call normev(tmpv, norme)
        call lcprsn(3, dir, tmpv, psca)
        if (abs(psca) .gt. 0.1d0) call u2mess('F', 'RUPTURE0_94')
    endif
    call getvid(motfac, 'DIRE_THETA', iocc, iarg, 1,&
                dirth, nbdir)
!
! 1ER CAS: LA DIRECTION DE THETA EST DONNEE, ON LA NORME
!
    if (direc) then
        norme = 0.d0
        do 991 i = 1, 3
            norme = norme + dir(i)*dir(i)
991      continue
        norme = sqrt(norme)
        do 1 i = 1, lobj2
            zr(in2+(i-1)*3+1-1) = dir(1)/norme
            zr(in2+(i-1)*3+2-1) = dir(2)/norme
            zr(in2+(i-1)*3+3-1) = dir(3)/norme
 1      continue
    else if (nbdir.ne.0) then
!
! 2ER CAS: LA DIRECTION DU CHAMP THETA EST DONNEE EN CHAQUE NOEUD
!          DU FOND DE FISSURE PAR L'UTILISATEUR
!
        call chpver('F', dirth(1:19), 'NOEU', 'DEPL_R', ierd)
        dirth(20:24) = '.VALE'
        call jeveuo(dirth, 'L', idirth)
        call jelira(dirth, 'LONMAX', lndir, k1bid)
        dirth(20:24) = '.REFE'
        call jeveuo(dirth, 'L', irefe)
        call jeveuo(zk24(irefe+1)(1:19)//'.DEEQ', 'L', ideeq)
        call assert(lndir.eq.(3*lobj2))
        do 5 i = 1, lobj2
            dirx = zr(idirth+(i-1)*3+1-1)
            diry = zr(idirth+(i-1)*3+2-1)
            dirz = zr(idirth+(i-1)*3+3-1)
            norme = sqrt(dirx*dirx + diry*diry + dirz*dirz)
            suiv = .false.
            do 6 j = 1, lobj2
                if (zi(iadnum+j-1) .eq. zi(ideeq+6*(i-1)+1-1)) then
                    zr(in2+(j-1)*3+1-1) = dirx/norme
                    zr(in2+(j-1)*3+2-1) = diry/norme
                    zr(in2+(j-1)*3+3-1) = dirz/norme
                    suiv = .true.
                endif
                if (suiv) goto 5
 6          continue
 5      continue
!
    else
!
! 3ER CAS: LA DIRECTION DE THETA EST CALCULEE, ON LA NORME
!
!  LEVRE SUPERIEURE
!
        if (ielsup .ne. 0) then
            call gdirec(noma, fond, 'LEVRESUP', nomno, nomnoe,&
                        coorn, lobj2, dire4, milieu)
            call jeveuo(dire4, 'L', idirs)
            if (ielinf .ne. 0) then
!
!  LEVRE INFERIEURE
!
                call gdirec(noma, fond, 'LEVREINF', nomno, nomnoe,&
                            coorn, lobj2, dire5, milieu)
                call jeveuo(dire5, 'L', idiri)
!
! LES DIRECTIONS OBTENUES POUR CHAQUE LEVRE SONT MOYENNEES ET NORMEES
!
                do 2 i = 1, lobj2
                    dirx = zr(idiri+(i-1)*3+1-1)
                    diry = zr(idiri+(i-1)*3+2-1)
                    dirz = zr(idiri+(i-1)*3+3-1)
                    vecx = (zr(idirs+(i-1)*3+1-1)+dirx)/2
                    vecy = (zr(idirs+(i-1)*3+2-1)+diry)/2
                    vecz = (zr(idirs+(i-1)*3+3-1)+dirz)/2
                    norme = sqrt(vecx*vecx + vecy*vecy + vecz*vecz)
                    zr(in2+(i-1)*3+1-1) = vecx/norme
                    zr(in2+(i-1)*3+2-1) = vecy/norme
                    zr(in2+(i-1)*3+3-1) = vecz/norme
 2              continue
            else
                do 22 i = 1, lobj2
                    dirx = zr(idirs+(i-1)*3+1-1)
                    diry = zr(idirs+(i-1)*3+2-1)
                    dirz = zr(idirs+(i-1)*3+3-1)
                    norme = sqrt(dirx*dirx + diry*diry + dirz*dirz)
                    zr(in2+(i-1)*3+1-1) = dirx/norme
                    zr(in2+(i-1)*3+2-1) = diry/norme
                    zr(in2+(i-1)*3+3-1) = dirz/norme
22              continue
            endif
        else if (ienorm.ne.0) then
            call gdinor(norm, lobj2, iadnum, coorn, in2)
        else
            call jeveuo(fond//'.BASEFOND', 'L', jvect)
            do 23 i = 1, nbnoff
                zr(in2+(i-1)*3+1-1) = zr(jvect-1+6*(i-1)+4)
                zr(in2+(i-1)*3+2-1) = zr(jvect-1+6*(i-1)+5)
                zr(in2+(i-1)*3+3-1) = zr(jvect-1+6*(i-1)+6)
23          continue
        endif
!
!  ON RECUPERE LES DIRECTIONS UTILISATEUR AUX EXTREMITES DU FOND(EN 3D)
!
        if (itanor .ne. 0) then
            call jeveuo(objor, 'L', iaorig)
            vecx = zr(iaorig)
            vecy = zr(iaorig+1)
            vecz = zr(iaorig+2)
            norme = sqrt(vecx*vecx + vecy*vecy + vecz*vecz)
            zr(in2+1-1) = vecx/norme
            zr(in2+2-1) = vecy/norme
            zr(in2+3-1) = vecz/norme
        endif
        if (itanex .ne. 0) then
            call jeveuo(objex, 'L', iaextr)
            vecx = zr(iaextr)
            vecy = zr(iaextr+1)
            vecz = zr(iaextr+2)
            norme = sqrt(vecx*vecx + vecy*vecy + vecz*vecz)
            zr(in2+3*(lobj2-1)+1-1) = vecx/norme
            zr(in2+3*(lobj2-1)+2-1) = vecy/norme
            zr(in2+3*(lobj2-1)+3-1) = vecz/norme
        endif
    endif
!
    call jeexin(fond//'.BASEFOND', iebas)
    if (iebas .ne. 0) then
        call jeveuo(fond//'.BASEFOND', 'L', jvect)
        zr(in2+1-1) = zr(jvect-1+4)
        zr(in2+2-1) = zr(jvect-1+5)
        zr(in2+3-1) = zr(jvect-1+6)
        zr(in2+(lobj2-1)*3+1-1) = zr(jvect-1+6*(nbnoff-1)+4)
        zr(in2+(lobj2-1)*3+2-1) = zr(jvect-1+6*(nbnoff-1)+5)
        zr(in2+(lobj2-1)*3+3-1) = zr(jvect-1+6*(nbnoff-1)+6)
    endif
!
! ALLOCATION D UN OBJET INDICATEUR DU CHAMP THETA SUR GAMMO
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbel,&
                chbid, ierd)
    indicg = '&&COURON.INDIC        '
    call wkvect(indicg, 'V V I', nbel, indic)
!
! ALLOCATION DES OBJETS POUR STOCKER LE CHAMP_NO THETA ET LA DIRECTION
! TYPE CHAM_NO ( DEPL_R) AVEC PROFIL NOEUD CONSTANT (3 DDL)
!
    chamno(1:19) = resu(1:19)
!
!  .DESC
    chamno(20:24) = '.DESC'
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nec,&
                k1bid, ibid)
    call wkvect(chamno, base//' V I', 2+nec, idesc)
!
    call jeecra(chamno, 'DOCU', 0, 'CHNO')
    call jenonu(jexnom('&CATA.GD.NOMGD', 'DEPL_R'), numa)
    zi(idesc+1-1) = numa
    zi(idesc+2-1) = -3
    zi(idesc+3-1) = 14
!
!  .REFE
    chamno(20:24) = '.REFE'
    call wkvect(chamno, base//' V K24', 4, irefe)
    zk24(irefe+1-1) = noma//'                '
!
!  .VALE
    chamno(20:24) = '.VALE'
    call wkvect(chamno, base//' V R', 3*nbel, itheta)
!
    do 4 i = 1, lobj2
        num = zi(iadnum+i-1)
        iadrtt = iadrt3 + i - 1
        zr(itheta+(num-1)*3+1-1) = zr(iadrtt)*zr(in2+(i-1)*3+1-1)
        zr(itheta+(num-1)*3+2-1) = zr(iadrtt)*zr(in2+(i-1)*3+2-1)
        zr(itheta+(num-1)*3+3-1) = zr(iadrtt)*zr(in2+(i-1)*3+3-1)
        zi(indic+num-1) = 1
 4  end do
!
! BOUCLE SUR LES NOEUDS M COURANTS DU MAILLAGE SANS GAMMO
! POUR CALCULER PROJ(M)=N
    do 500 i = 1, nbel
        if (zi(indic+i-1) .ne. 1) then
            xm = zr(iadrco+(i-1)*3+1-1)
            ym = zr(iadrco+(i-1)*3+2-1)
            zm = zr(iadrco+(i-1)*3+3-1)
            dmin = 10000000.d0
            jmin = 0
            smin = 0.d0
            do 600 j = 1, lobj2-1
                xi1 = zr(iadrco+(zi(iadnum+j-1)-1)*3+1-1)
                yi1 = zr(iadrco+(zi(iadnum+j-1)-1)*3+2-1)
                zi1 = zr(iadrco+(zi(iadnum+j-1)-1)*3+3-1)
                xj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+1-1)
                yj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+2-1)
                zj1 = zr(iadrco+(zi(iadnum+j+1-1)-1)*3+3-1)
                xij = xj1-xi1
                yij = yj1-yi1
                zij = zj1-zi1
                xim = xm-xi1
                yim = ym-yi1
                zim = zm-zi1
                s = xij*xim + yij*yim + zij*zim
                norm2 = xij*xij + yij *yij + zij*zij
                s = s/norm2
                if ((s-1) .ge. eps) then
                    s = 1.d0
                endif
                if (s .le. eps) then
                    s = 0.d0
                endif
                xn = s*xij+xi1
                yn = s*yij+yi1
                zn = s*zij+zi1
                d = sqrt((xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+(zn-zm)*(zn- zm))
                if (d .lt. (dmin*(1-abs(r8prem())*100))) then
                    dmin = d
                    jmin = j
                    smin = s
                endif
600          continue
            rii = (1-smin)*zr(iadrt1+jmin-1)+smin*zr(iadrt1+jmin+1-1)
            rsi = (1-smin)*zr(iadrt2+jmin-1)+smin*zr(iadrt2+jmin+1-1)
            alpha = (dmin-rii)/(rsi-rii)
            iadrtt = iadrt3+jmin-1
            tei = zr(iadrtt)
            tej = zr(iadrtt+1)
            valx = (1-smin)*zr(in2+(jmin-1)*3+1-1)*tei
            valx = valx+smin*zr(in2+(jmin+1-1)*3+1-1)*tej
            valy = (1-smin)*zr(in2+(jmin-1)*3+2-1)*tei
            valy = valy+smin*zr(in2+(jmin+1-1)*3+2-1)*tej
            valz = (1-smin)*zr(in2+(jmin-1)*3+3-1)*tei
            valz = valz+smin*zr(in2+(jmin+1-1)*3+3-1)*tej
            if ((abs(alpha).le.eps) .or. (alpha.lt.0)) then
                zr(itheta+(i-1)*3+1-1) = valx
                zr(itheta+(i-1)*3+2-1) = valy
                zr(itheta+(i-1)*3+3-1) = valz
            else if ((abs(alpha-1).le.eps).or.((alpha-1).gt.0)) then
                zr(itheta+(i-1)*3+1-1) = 0.d0
                zr(itheta+(i-1)*3+2-1) = 0.d0
                zr(itheta+(i-1)*3+3-1) = 0.d0
            else
                zr(itheta+(i-1)*3+1-1) = (1-alpha)*valx
                zr(itheta+(i-1)*3+2-1) = (1-alpha)*valy
                zr(itheta+(i-1)*3+3-1) = (1-alpha)*valz
            endif
        endif
500  end do
!
! DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jeexin(dire4, iret)
    if (iret .ne. 0) then
        call jedetr(dire4)
    endif
    call jeexin(dire5, iret)
    if (iret .ne. 0) then
        call jedetr(dire5)
    endif
    call jedetr(indicg)
    call jedetr(numgam)
!
    call jedema()
!
end subroutine
