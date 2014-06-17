subroutine rec110(nomres, nomsqu, modgen)
    implicit none
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
!***********************************************************************
!-----------------------------------------------------------------------
!  BUT : < FUSIONNER LES NOEUDS D'INTERFACE D'UN SQUELETTE EXISTANT>
!
!
!-----------------------------------------------------------------------
!
! NOMRES  /I/ : NOM K8 DU MAILLAGE SQUELETTE A MODIFIER
! MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
!
!   PARAMETER : REPRESENTE LE NOMBRE MAX DE COMPOSANTES DE LA GRANDEUR
!   SOUS-JACENTE TRAITEE
!
    character(len=24) :: valk(4)
    character(len=8) :: nomres, nomsqu, modgen, tt, lintf, ljntf
    character(len=8) :: k8bid, nomnoe, crit, nomsst
    logical :: fusion
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, i2, iadres, ibid, igr, in
    integer :: incr, inew, iocc, iold, iposi, ireco
    integer :: istac,  jn, jncr, jposi, jstac, lconn
    integer ::  lcorr, lcort, ldime, lintd, linver, ljntd
    integer ::  lsk, lsk2,  ltabi, ltabj, lvnew
    integer ::  nbcoor, nbec, nbfuse, nbma, nbmoin, nbn
    integer :: nbnd, nbnd2, nbnew, nbni, nbnj, nbocc, nbreco
    integer :: nbstac, ndist, nnodes, nr, numero
    real(kind=8) :: dist, distij, prec, xii, xj, yii, yj
    real(kind=8) :: zii, zj
    character(len=24), pointer :: ltns(:) => null()
    character(len=8), pointer :: vnomsst(:) => null()
    real(kind=8), pointer :: nlcoord(:) => null()
    real(kind=8), pointer :: nlvold(:) => null()
    character(len=24), pointer :: refe(:) => null()
!-----------------------------------------------------------------------
    data tt      /'&&REC110'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECOPIE DE DES OBJETS SQUELETTE DANS LE NOUVEAU SQUELETTE ---
    call copisd('SQUELETTE', 'G', nomsqu, nomres)
!     IL FAUT MODIFIER .LTNS(1) :
    call jeveuo(nomres//'           .LTNS', 'E', vk24=ltns)
    ltns(1)(1:8)=nomres
!
    call jelira(nomres//'         .NOMSST', 'LONUTI', nbstac)
    call jeveuo(nomres//'         .NOMSST', 'L', vk8=vnomsst)
    call jeveuo(nomres//'.COORDO    .VALE', 'E', vr=nlcoord)
    call jelira(nomres//'.COORDO    .VALE', 'LONUTI', nbcoor)
    call jelira(nomres//'.INV.SKELETON   ', 'LONUTI', nbnd2)
    call jeveuo(nomres//'.INV.SKELETON   ', 'E', lsk)
    nbnd = nbnd2/2
    lsk2 = lsk + nbnd
!
!     --- CREATION D'UNE LISTE DE CORRESPONDANCE ANCIEN/NOUVEAU NOEUD
    call wkvect(tt//'.CORRES', 'V V I', nbnd, lcort)
!     ET INVERSE
    call wkvect(tt//'.INVER', 'V V I', nbnd, linver)
    do i = 1, nbnd
        zi(lcort-1+i) = i
        zi(linver-1+i) = i
    end do
!
!
!     --- TABLEAU DE LISTES DE NOEUDS POUR COMPARAISON ---
    call wkvect(tt//'.TABI', 'V V I', nbnd, ltabi)
    call wkvect(tt//'.TABJ', 'V V I', nbnd, ltabj)
!
! --- LECTURE DE RECO_GLOBAL ---
!
!
    call getvtx('RECO_GLOBAL', 'GROUP_NO_1', iocc=1, nbval=0, nbret=igr)
    if (igr .eq. 0) then
!
! --- ON FUSIONNE LES NOEUDS DE TOUTES LES INTERFACES DYNAMIQUES ---
!
!     --- LECTURE DE LA PRECISION
        call getvr8('RECO_GLOBAL', 'PRECISION', iocc=1, scal=prec, nbret=ibid)
        call getvr8('RECO_GLOBAL', 'DIST_REFE', iocc=1, scal=dist, nbret=ndist)
        call getvtx('RECO_GLOBAL', 'CRITERE', iocc=1, scal=crit, nbret=ndist)
        if (ndist .eq. 0) then
!     --- AU CAS OU LA DISTANCE DE REFERENCE N'EST PAS DONNEE,ON DEVRAIT
!         LA LIRE DANS LA SD MAILLAGE (VOIR COMMANDE LIRE_MAILLAGE).
!         ELLE EST POUR L'INSTANT OBLIGATOIRE A LA PREMIERE OCCURENCE
!         DE RECO_GLOBAL
            call utmess('F', 'ALGORITH10_21')
        endif
        incr = 0
        nbfuse = 0
        nbmoin = 0
        do istac = 1, nbstac
            nomsst = vnomsst(istac)
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        lintf)
            call jelira(lintf//'.IDC_DEFO', 'LONUTI', nnodes)
            call dismoi('NB_EC', lintf, 'INTERF_DYNA', repi=nbec)
            nbni = nnodes/(2+nbec)
            call jeveuo(lintf//'.IDC_DEFO', 'L', lintd)
            do in = 1, nbni
                numero = zi(lintd-1+in)
                if (zi(lsk-1+numero+incr) .ne. istac) then
                    call utmess('E+', 'ALGORITH15_55')
                    call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                    valk (1) = k8bid
                    valk (2) = lintf
                    valk (3) = nomsst
                    call utmess('E', 'ALGORITH15_56', nk=3, valk=valk)
                endif
                if (zi(lsk2-1+numero+incr) .ne. numero) then
                    call utmess('E+', 'ALGORITH15_55')
                    call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                    valk (1) = k8bid
                    valk (2) = lintf
                    valk (3) = nomsqu
                    call utmess('E', 'ALGORITH15_58', nk=3, valk=valk)
!
                endif
                zi(ltabi-1+in) = numero + incr
            end do
            incr = numero + incr
!        --- ON SE POSITIONNE EN FIN DE SOUS-STRUCTURE ISTAC ---
 20         continue
            if (zi(lsk+incr) .eq. istac) then
                incr = incr + 1
                goto 20
            endif
!        ---
            jncr = incr
            do jstac = istac+1, nbstac
                nomsst = vnomsst(jstac)
                call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                            ljntf)
                call jelira(ljntf//'.IDC_DEFO', 'LONUTI', nnodes)
                call dismoi('NB_EC', ljntf, 'INTERF_DYNA', repi=nbec)
                nbnj = nnodes/(2+nbec)
                call jeveuo(ljntf//'.IDC_DEFO', 'L', ljntd)
                do jn = 1, nbnj
                    numero = zi(ljntd-1+jn)
                    if (zi(lsk-1+numero+jncr) .ne. jstac) then
                        call utmess('E+', 'ALGORITH15_55')
                        call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                        valk (1) = k8bid
                        valk (2) = ljntf
                        valk (3) = nomsst
                        call utmess('E', 'ALGORITH15_60', nk=3, valk=valk)
                    endif
                    if (zi(lsk2-1+numero+jncr) .ne. numero) then
                        call utmess('E+', 'ALGORITH15_55')
                        call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                        valk (1) = k8bid
                        valk (2) = ljntf
                        valk (3) = nomsqu
                        call utmess('E', 'ALGORITH15_62', nk=3, valk=valk)
                    endif
                    zi(ltabj-1+jn) = numero + jncr
                end do
                jncr = numero + jncr
!           --- ON SE POSITIONNE EN FIN DE SOUS-STRUCTURE JSTAC ---
 40             continue
                if (zi(lsk+jncr) .eq. jstac) then
                    jncr = jncr + 1
                    goto 40
                endif
!           ---- FUSION DES NOEUDS ---
                do in = 1, nbni
                    iposi = (zi(ltabi-1+in)-1)*3
                    xii = nlcoord(iposi+1)
                    yii = nlcoord(1+iposi+1)
                    zii = nlcoord(1+iposi+2)
                    do jn = 1, nbnj
                        fusion = .true.
                        jposi = (zi(ltabj-1+jn)-1)*3
                        xj = nlcoord(jposi+1)
                        yj = nlcoord(1+jposi+1)
                        zj = nlcoord(1+jposi+2)
                        distij = (xii-xj)**2+(yii-yj)**2+(zii-zj)**2
                        distij = sqrt(abs(distij))
                        if (crit .eq. 'RELATIF') then
                            if (distij .gt. prec*dist) fusion=.false.
                        else
                            if (distij .gt. dist) fusion=.false.
                        endif
                        if (fusion) then
                            i1 = zi(lcort-1+zi(ltabi-1+in))
                            i2 = zi(ltabi-1+in)
                            if (i1 .eq. i2) nbmoin = nbmoin +1
                            zi(lcort-1+zi(ltabi-1+in)) = zi(ltabj-1+ jn)
                            zi(linver-1+zi(ltabj-1+jn)) = zi(ltabi-1+ in)
                            nbfuse = nbfuse + 1
                        endif
                    end do
                end do
            end do
        end do
!
    else
!
! --- ON FUSIONNE LES INTERFACES DONNEES PAR L'UTILISATEUR ---
!
        call getfac('RECO_GLOBAL', nbreco)
        nbfuse = 0
        nbmoin = 0
        do ireco = 1, nbreco
            call getvtx('RECO_GLOBAL', 'GROUP_NO_1', iocc=ireco, nbval=0, nbret=nr)
            if (nr .eq. 0) then
                call utmess('F+', 'ALGORITH15_63')
            endif
!        --- LECTURE DE LA PRECISION
            call getvr8('RECO_GLOBAL', 'PRECISION', iocc=ireco, scal=prec, nbret=ibid)
            call getvr8('RECO_GLOBAL', 'DIST_REFE', iocc=ireco, scal=dist, nbret=ndist)
            call getvtx('RECO_GLOBAL', 'CRITERE', iocc=ireco, scal=crit, nbret=ndist)
            if (ndist .eq. 0) then
!        --- AU CAS OU LA DISTANCE DE REFERENCE N'EST PAS DONNEE,
!            ON DEVRAIT LA LIRE DANS LA SD MAILLAGE.
                crit='RELATIF'
            endif
            call getvtx('RECO_GLOBAL', 'SOUS_STRUC_1', iocc=ireco, scal=nomsst, nbret=ibid)
!        --- RECHERCHE DE LA SOUS-STRUCTURE ---
            istac = 0
 90         continue
            istac = istac + 1
            if (istac .le. nbstac) then
                if (vnomsst(istac) .ne. nomsst) goto 90
            endif
            if (istac .gt. nbstac) then
                valk (1) = nomsst
                call utmess('F', 'ALGORITH15_64', sk=valk(1))
            endif
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        lintf)
            call jenuno(jexnum(lintf//'.IDC_NOMS', 2), k8bid)
            call getvtx('RECO_GLOBAL', 'GROUP_NO_1', iocc=ireco, scal=nomnoe, nbret=ibid)
            if (nomnoe .ne. k8bid) then
                valk (1) = lintf
                valk (2) = nomsst
                valk (3) = k8bid
                valk (4) = nomnoe
                call utmess('E', 'ALGORITH15_65', nk=4, valk=valk)
            endif
            call jelira(lintf//'.IDC_DEFO', 'LONUTI', nnodes)
            call dismoi('NB_EC', lintf, 'INTERF_DYNA', repi=nbec)
            nbni = nnodes/(2+nbec)
!         NBNI = NNODES/3
            call jeveuo(lintf//'.IDC_DEFO', 'L', lintd)
            incr = 0
!        --- ON SE POSITIONNE AVANT ISTAC ---
100         continue
            if (zi(lsk+incr) .ne. istac) then
                incr = incr + 1
                goto 100
            endif
            do in = 1, nbni
                zi(ltabi-1+in) = zi(lintd-1+in) + incr
            end do
            call getvtx('RECO_GLOBAL', 'SOUS_STRUC_2', iocc=ireco, scal=nomsst, nbret=ibid)
!        --- RECHERCHE DE LA SOUS-STRUCTURE ---
            jstac = 0
120         continue
            jstac = jstac + 1
            if (jstac .le. nbstac) then
                if (vnomsst(jstac) .ne. nomsst) goto 120
            endif
            if (jstac .gt. nbstac) then
                valk (1) = nomsst
                call utmess('F', 'ALGORITH15_66', sk=valk(1))
            endif
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        ljntf)
            call jenuno(jexnum(ljntf//'.IDC_NOMS', 1), k8bid)
            call getvtx('RECO_GLOBAL', 'GROUP_NO_2', iocc=ireco, scal=nomnoe, nbret=ibid)
            if (nomnoe .ne. k8bid) then
                valk (1) = ljntf
                valk (2) = nomsst
                valk (3) = k8bid
                valk (4) = nomnoe
                call utmess('E', 'ALGORITH15_67', nk=4, valk=valk)
            endif
            call jelira(ljntf//'.IDC_DEFO', 'LONUTI', nnodes)
            call dismoi('NB_EC', ljntf, 'INTERF_DYNA', repi=nbec)
            nbnj = nnodes/(2+nbec)
!         NBNJ = NNODES/3
            call jeveuo(ljntf//'.IDC_DEFO', 'L', ljntd)
            jncr = 0
!        --- ON SE POSITIONNE AVANT JSTAC ---
130         continue
            if (zi(lsk+jncr) .ne. jstac) then
                jncr = jncr + 1
                goto 130
            endif
            do jn = 1, nbnj
                zi(ltabj-1+jn) = zi(ljntd-1+jn) + jncr
            end do
!           ---- FUSION DES NOEUDS ---
            do in = 1, nbni
                iposi = (zi(ltabi-1+in)-1)*3
                xii = nlcoord(iposi+1)
                yii = nlcoord(1+iposi+1)
                zii = nlcoord(1+iposi+2)
                do jn = 1, nbnj
                    fusion = .true.
                    jposi = (zi(ltabj-1+jn)-1)*3
                    xj = nlcoord(jposi+1)
                    yj = nlcoord(1+jposi+1)
                    zj = nlcoord(1+jposi+2)
                    distij = (xii-xj)**2+(yii-yj)**2+(zii-zj)**2
                    distij = sqrt(abs(distij))
                    if (crit .eq. 'RELATIF') then
                        if (distij .gt. prec*dist) fusion=.false.
                    else
                        if (distij .gt. dist) fusion=.false.
                    endif
                    if (fusion) then
                        i1 = zi(lcort-1+zi(ltabi-1+in))
                        i2 = zi(ltabi-1+in)
                        if (i1 .eq. i2) nbmoin = nbmoin +1
                        zi(lcort-1+zi(ltabi-1+in)) = zi(ltabj-1+jn)
                        zi(linver-1+zi(ltabj-1+jn)) = zi(ltabi-1+in)
                        nbfuse = nbfuse + 1
                    endif
                end do
            end do
        end do
!
! --- FIN DE TRAITEMENT DES OCCURENCES DE RECO_GLOBAL ---
    endif
!
! --- REAJUSTEMENT DE .CORRES ET NOUVEL OBJET DANS LE SQUELETTE ---
!      NBNEW = NBND - NBFUSE
    nbnew = nbnd - nbmoin
    call wkvect(nomres//'.CORRES', 'G V I', nbnew, lcorr)
!     --- MISE EN PLACE DU TABLEAU DE CORRESPONDANCE ---
    inew = 1
    iold = 1
200 continue
    if (inew .le. nbnew .and. iold .le. nbnd) then
        if (zi(linver-1+iold) .ne. iold) then
!           --- NOEUD FUSIONNE : N'EXISTE PLUS DANS LE NOUVEAU MAILLAGE
            iold = iold + 1
            goto 200
        endif
!        --- RECHERCHE DU NOEUD CORRESPONDANT LE PLUS ELOIGNE ---
!            (C'EST CELUI-LA QU'ON RETIENT)
        iadres = zi(lcort-1+iold)
202     continue
        if (iadres .ne. zi(lcort-1+iadres)) then
            iadres = zi(lcort-1+iadres)
            goto 202
        endif
!        --- NOEUD CORRESPONDANT ---
        zi(lcorr-1+inew) = iadres
!        --- ON RETIENT SON INVERSE ---
        zi(linver-1+iadres) = inew
!
        inew = inew + 1
        iold = iold + 1
        goto 200
    endif
!     --- MISE EN PLACE DU TABLEAU INVERSE ---
    do iold = 1, nbnd
        if (zi(lcort-1+iold) .ne. iold) then
!        --- NOEUD FUSIONNE : ON RECHERCHE SON INVERSE ---
!        --- RECHERCHE DU NOEUD CORRESPONDANT LE PLUS ELOIGNE ---
!            (IL DEVIENT LA REFERENCE)
            iadres = zi(lcort-1+iold)
205         continue
            if (iadres .ne. zi(lcort-1+iadres)) then
                iadres = zi(lcort-1+iadres)
                goto 205
            endif
            zi(linver-1+iold) = zi(linver-1+iadres)
        endif
    end do
!
! --- MISE A JOUR DES OBJETS DU NOUVEAU SQUELETTE ---
! --- COLLECTION .CONNEX
    call jeveuo(nomres//'.DIME', 'L', ldime)
!     NOMBRE DE MAILLES NBMA
    nbma = zi(ldime+2)
    call jelira(nomres//'.CONNEX', 'NUTIOC', nbocc)
    do iocc = 1, nbma
        call jeveuo(jexnum(nomres//'.CONNEX', iocc), 'E', lconn)
        call jelira(jexnum(nomres//'.CONNEX', iocc), 'LONMAX', nbn)
        do i = 1, nbn
            zi(lconn-1+i) = zi(linver-1+ zi(lconn-1+i))
        end do
    end do
!
! --- OBJET  .REFE
    call jeveuo(nomres//'.COORDO    .REFE', 'E', vk24=refe)
    refe(1) = nomres
!
! --- OBJET  .DIME
    call jeveuo(nomres//'.DIME', 'E', ldime)
    zi(ldime) = nbnew
!
! --- OBJET  .VALE
    call jedetr(nomres//'.COORDO    .VALE')
    call wkvect(nomres//'.COORDO    .VALE', 'G V R', nbnew*3, lvnew)
    call jeveuo(nomsqu//'.COORDO    .VALE', 'L', vr=nlvold)
    do i = 1, nbnew
        iold = zi(lcorr-1+i)
        zr(lvnew-1+(i-1)*3+1) = nlvold((iold-1)*3+1)
        zr(lvnew-1+(i-1)*3+2) = nlvold((iold-1)*3+2)
        zr(lvnew-1+(i-1)*3+3) = nlvold((iold-1)*3+3)
    end do
!
! --- OBJET .NOMNOE
    call jedetr(nomres//'.NOMNOE')
    call jecreo(nomres//'.NOMNOE', 'G N K8')
    call jeecra(nomres//'.NOMNOE', 'NOMMAX', nbnew)
    do i = 1, nbnew
        iold = zi(lcorr-1+i)
        call jenuno(jexnum(nomsqu//'.NOMNOE', iold), nomnoe)
        call jecroc(jexnom(nomres//'.NOMNOE', nomnoe))
    end do
!
    call jedetr(tt//'.TABI')
    call jedetr(tt//'.TABJ')
    call jedetr(tt//'.CORRES')
    call jedetr(tt//'.INVER')
    call jedema()
end subroutine
