subroutine carbe3(charge)
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
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/mgauss.h"
#include "asterfort/pmppr.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utbtab.h"
#include "asterfort/wkvect.h"
    character(len=8) :: charge
!
!     TRAITER LE MOT CLE LIAISON_RBE3 DE AFFE_CHAR_MECA
!     ET ENRICHIR LA CHARGE (CHARGE) AVEC LES RELATIONS LINEAIRES
! IN/JXVAR : CHARGE : NOM D'UNE SD CHARGE
! ----------------------------------------------------------------------
    integer :: vali(2)
!
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=4) :: typcoe, typval
    character(len=7) :: typcha
    character(len=8) :: k8bid, mode, noma, nomres, noemai, nomnoe, ddltrr(6)
    character(len=8) :: ddlcod, ddlmac(6), betaf, numlag
    character(len=15) :: coordo
    character(len=16) :: motfac, concep, nomcmd
    character(len=19) :: lisrel
    character(len=24) :: ddlstr, grouno, noeuma, gromai
    complex(kind=8) :: betac
    integer :: ifm, niv, ibid, iarg, ier, iret
    integer :: idxrbe, idxlig, idxcol, idxvec, idxnoe, idxgro, idxter
    integer :: idxddl
    integer :: posesc, posmai, cntlig, cntddl, cntnoe, inilig
    integer :: jcoor, jlises, jcofes, jddles, jcescl, jcoore, jw, js, jb
    integer :: jnorel, jddl, jcmur, jcmuc, jcmuf, jdirec, jdime
    integer :: jnogro, jnoesc, jxab, jnzddl, jnznor
    integer :: nbrbe3, nbdles, nbcfes, nbddl, nblign, nbcol, nbgrou, nbent
    integer :: nbnoeu, nbdlma, maxesc, maxles, maxddl, dime
    logical :: fincod, ddlesc(6), ddlmai(6), frstco, dime2d
    real(kind=8) :: rbid, coomai(3), cooesc(3), lc, norme, lcsqua, stws(6, 6)
    real(kind=8) :: cofesc, beta, x(6, 6)
! ----------------------------------------------------------------------
!
    motfac = 'LIAISON_RBE3    '
    call getfac(motfac, nbrbe3)
!
    if (nbrbe3 .eq. 0) goto 999
!
    beta = 0.0d0
    betac = (1.0d0,0.0d0)
    typcoe = 'REEL'
    typval = 'REEL'
    ddltrr(1) = 'DX'
    ddltrr(2) = 'DY'
    ddltrr(3) = 'DZ'
    ddltrr(4) = 'DRX'
    ddltrr(5) = 'DRY'
    ddltrr(6) = 'DRZ'
!
    call infniv(ifm, niv)
    call jemarq()
!
    if (niv .eq. 2) then
        write(ifm,*) 'NOMBRE RELATIONS RBE3 : ',nbrbe3
!       CALL JXVERI(' ')
    endif
!
    call getres(nomres, concep, nomcmd)
    call dismoi('F', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                typcha, ier)
    call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                mode, ier)
    call dismoi('F', 'DIM_GEOM', mode, 'MODELE', dime,&
                k8bid, ier)
    dime2d=(dime.eq.2)
    if (niv .eq. 2) then
        write(ifm,*) 'MODELE 2D : ',dime2d
    endif
!
    call dismoi('F', 'NOM_MAILLA', charge, 'CHARGE', ibid,&
                noma, ier)
!
    noeuma = noma//'.NOMNOE'
    grouno = noma//'.GROUPENO'
    coordo = noma//'.COORDO'
    call jeveuo(coordo//'    .VALE', 'L', jcoor)
!
!     -- CALCUL DE MAXLES : NBRE DE TERMES MAXI D'UNE LISTE
!        DE GROUP_NO_ESCL OU DE NOEUD_ESCL
!        --------------------------------------------------
    maxles = 0
    do idxrbe = 1, nbrbe3
        call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_ESCL', idxrbe,&
                    iarg, 0, k8bid, nbgrou)
        maxles = max(maxles,-nbgrou)
        call getvem(noma, 'NOEUD', motfac, 'NOEUD_ESCL', idxrbe,&
                    iarg, 0, k8bid, nbnoeu)
        maxles = max(maxles,-nbnoeu)
    end do
!
    if (maxles .eq. 0) then
        call u2mess('F', 'MODELISA10_7')
    endif
    call wkvect('&&CARBE3.LISTESCL', 'V V K24', maxles, jlises)
!
!     -- CALCUL DE MAXDDL ET VERIFICATION DES NOEUDS ET GROUP_NO
!        MAXDDL EST LE NOMBRE MAXI DE NOEUDS IMPLIQUES DANS UNE
!        RELATION LINEAIRE
!        -------------------------------------------------------
    maxddl = 0
    maxesc = 0
    do idxrbe = 1, nbrbe3
!
        call getvtx(motfac, 'GROUP_NO_ESCL', idxrbe, iarg, maxles,&
                    zk24(jlises), nbgrou)
        if (nbgrou .ne. 0) then
            do idxgro = 1, nbgrou
                call jelira(jexnom(grouno, zk24(jlises-1+idxgro)), 'LONUTI', nbnoeu, k1bid)
                if (nbnoeu .eq. 0) then
                    call u2mesk('F', 'MODELISA10_8', 1, zk24(jlises-1+ idxgro))
                endif
                maxesc = max(maxesc, nbnoeu)
            enddo
        else
            call getvtx(motfac, 'NOEUD_ESCL', idxrbe, iarg, 0,&
                        k8bid, nbnoeu)
            nbnoeu = -nbnoeu
            maxesc = max(maxesc, nbnoeu)
        endif
!
        cntddl = 1
        cntddl = cntddl + maxesc * 6
        maxddl = max(maxddl,cntddl)
    end do
!
    if (niv .eq. 2) then
        write(ifm,*) 'MAXESC : ',maxesc
        write(ifm,*) 'MAXDDL : ',maxddl
    endif
!
!     -- ALLOCATION DES TABLEAUX DE TRAVAIL
!     -------------------------------------
    lisrel = '&&CARBE3.RLLISTE'
    call wkvect('&&CARBE3.LISNOREL', 'V V K8', maxddl, jnorel)
    call wkvect('&&CARBE3.LISNZNOR', 'V V K8', maxddl, jnznor)
    call wkvect('&&CARBE3.LISNOESC', 'V V K8', maxesc, jnoesc)
    call wkvect('&&CARBE3.DDL  ', 'V V K8', maxddl, jddl)
    call wkvect('&&CARBE3.NZDDL', 'V V K8', maxddl, jnzddl)
    call wkvect('&&CARBE3.COEMUR', 'V V R', maxddl, jcmur)
    call wkvect('&&CARBE3.COEMUC', 'V V C', maxddl, jcmuc)
    call wkvect('&&CARBE3.COEMUF', 'V V K8', maxddl, jcmuf)
    call wkvect('&&CARBE3.DIRECT', 'V V R', 3*maxddl, jdirec)
    call wkvect('&&CARBE3.DIMENSION', 'V V I', maxddl, jdime)
    call wkvect('&&CARBE3.CESCL', 'V V L', maxesc*6, jcescl)
    call wkvect('&&CARBE3.COEFES', 'V V R', maxesc, jcofes)
    call wkvect('&&CARBE3.COOREL', 'V V R', maxesc*3, jcoore)
    call wkvect('&&CARBE3.DDLESCL', 'V V K24', maxesc, jddles)
!
!     BOUCLE SUR LES RELATIONS RBE3
!     -----------------------------------
    do  idxrbe = 1, nbrbe3
        if (niv .eq. 2) then
            write(ifm,*) 'INDEX RELATION RBE3 : ',idxrbe
        endif
!
        call getvtx(motfac, 'GROUP_NO_MAIT', idxrbe, iarg, 0,&
                    k8bid, nbent)
        nbent = -nbent
        if (nbent .ne. 0) then
            call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_MAIT', idxrbe,&
                        iarg, 1, gromai, nbent)
            call jeveuo(jexnom(grouno, gromai), 'L', jnogro)
            call jelira(jexnom(grouno, gromai), 'LONUTI', nbent, k1bid)
            if (nbent .ne. 1) then
                call u2mesg('F', 'MODELISA10_9', 1, gromai, 1,&
                            nbent, 0, rbid)
            endif
            call jenuno(jexnum(noeuma, zi(jnogro-1+1)), noemai)
        endif
!
        call getvtx(motfac, 'NOEUD_MAIT', idxrbe, iarg, 0,&
                    k8bid, nbent)
        if (nbent .ne. 0) then
            call getvem(noma, 'NOEUD', motfac, 'NOEUD_MAIT', idxrbe,&
                        iarg, 1, noemai, nbent)
        endif
!
        call jenonu(jexnom(noma//'.NOMNOE', noemai), posmai)
        coomai(1) = zr(jcoor-1+3*(posmai-1)+1)
        coomai(2) = zr(jcoor-1+3*(posmai-1)+2)
        coomai(3) = zr(jcoor-1+3*(posmai-1)+3)
        if (niv .eq. 2) then
            write(ifm,*) 'COORDS : ',coomai, ' DU NOEUD MAITRE : ',&
            noemai
        endif
!
        call getvtx(motfac, 'DDL_MAIT', idxrbe, iarg, 6,&
                    ddlmac, nbdlma)
!
        do  idxlig = 1, 6
            ddlmai(idxlig) = .false.
        enddo
!
        do  idxlig = 1, nbdlma
            ddlcod = ddlmac(idxlig)(1:lxlgut(ddlmac(idxlig)))
            if (ddltrr(1) .eq. ddlcod) then
                ddlmai(1) = .true.
            else if (ddltrr(2).eq.ddlcod) then
                ddlmai(2) = .true.
            else if (ddltrr(3).eq.ddlcod) then
                ddlmai(3) = .true.
            else if (ddltrr(4).eq.ddlcod) then
                ddlmai(4) = .true.
            else if (ddltrr(5).eq.ddlcod) then
                ddlmai(5) = .true.
            else if (ddltrr(6).eq.ddlcod) then
                ddlmai(6) = .true.
            endif
        enddo
!
        call getvtx(motfac, 'GROUP_NO_ESCL', idxrbe, iarg, 0,&
                    k8bid, nbgrou)
        if (nbgrou .ne. 0) then
            nbgrou = -nbgrou
            nbnoeu = 0
            call getvtx(motfac, 'GROUP_NO_ESCL', idxrbe, iarg, nbgrou,&
                        zk24(jlises), nbent)
            cntnoe = 0
            do  idxgro = 1, nbgrou
                call jeveuo(jexnom(grouno, zk24(jlises-1+idxgro)), 'L', jnogro)
                call jelira(jexnom(grouno, zk24(jlises-1+idxgro)), 'LONUTI', nbent, k1bid)
                nbnoeu = nbnoeu + nbent
                do  idxnoe = 1, nbent
                    cntnoe = cntnoe + 1
                    call jenuno(jexnum(noeuma, zi(jnogro-1+idxnoe)), nomnoe)
                    zk8(jnoesc+cntnoe-1) = nomnoe
                enddo
            enddo
        endif
!
        call getvtx(motfac, 'NOEUD_ESCL', idxrbe, iarg, 0,&
                    k8bid, nbent)
        if (nbent .ne. 0) then
            nbnoeu = -nbent
            call getvtx(motfac, 'NOEUD_ESCL', idxrbe, iarg, nbnoeu,&
                        zk8(jnoesc), nbent)
        endif
!
        if (niv .eq. 2) then
            write(ifm,*) 'LISTE DES ',nbnoeu, ' NOEUDS ESCLAVES'
            write(ifm,*) (zk8(jnoesc+idxlig-1),idxlig=1,nbnoeu)
        endif
!
        call getvtx(motfac, 'DDL_ESCL', idxrbe, iarg, nbnoeu,&
                    zk24(jddles), nbddl)
!
        if (nbddl .ne. 1 .and. nbddl .ne. nbnoeu) then
            vali(1) = nbddl
            vali(2) = nbnoeu
            call u2mesi('F', 'MODELISA10_10', 2, vali)
        endif
!
!       BOUCLE SUR LES NOEUDS ESCLAVES POUR EXTRAIRE LES DDLS
!       -----------------------------------------------------
        nbdles = 0
        do  idxnoe = 1, nbnoeu
            if (nbddl .gt. 1 .or. idxnoe .eq. 1) then
                if (nbddl .eq. 1) then
                    ddlstr = zk24(jddles-1+1)
                else
                    ddlstr = zk24(jddles-1+1)
                endif
!
!           EXTRACTION DDL_ESCL
!           -------------------------------------------------------
                do  idxlig = 1, 6
                    ddlesc(idxlig) = .false.
                enddo
!
                idxcol = 1
                do  idxlig = 1, lxlgut(ddlstr)
                    if (ddlstr(idxlig:idxlig) .eq. '-') then
                        ddlcod = ddlstr(idxcol:idxlig-1)
                        idxcol = idxlig + 1
                        fincod = .true.
                    else if (idxlig.eq.lxlgut(ddlstr)) then
                        ddlcod = ddlstr(idxcol:idxlig)
                        fincod = .true.
                    else
                        fincod = .false.
                    endif
                    if (fincod) then
                        if (ddltrr(1) .eq. ddlcod) then
                            ddlesc(1) = .true.
                        else if (ddltrr(2).eq.ddlcod) then
                            ddlesc(2) = .true.
                        else if (ddltrr(3).eq.ddlcod) then
                            ddlesc(3) = .true.
                        else if (ddltrr(4).eq.ddlcod) then
                            ddlesc(4) = .true.
                        else if (ddltrr(5).eq.ddlcod) then
                            ddlesc(5) = .true.
                        else if (ddltrr(6).eq.ddlcod) then
                            ddlesc(6) = .true.
                        else
                            call u2mesk('F', 'MODELISA10_11', 1, ddlcod)
                        endif
                    endif
                 enddo
            endif
            do  idxlig = 1, 6
                if (ddlesc(idxlig)) then
                    nbdles = nbdles + 1
                    zk8(jnorel-1+nbdles) = zk8(jnoesc-1+idxnoe)
                    zk8(jddl-1+nbdles) = ddltrr(idxlig)
                endif
                zl(jcescl-1+(idxnoe-1)*6+idxlig) = ddlesc(idxlig)
            enddo
        enddo
!
        if (niv .eq. 2) then
            write (ifm,*) 'NOMBRE DDL NOEUDS ESCLAVES : ',nbdles
        endif
!
!       BOUCLE SUR LES NOEUDS ESCLAVES POUR CALCULER Lc
!       -----------------------------------------------
        lc = 0
        do idxnoe = 1, nbnoeu
            call jenonu(jexnom(noma//'.NOMNOE', zk8(jnoesc-1+idxnoe)), posesc)
            cooesc(1) = zr(jcoor-1+3*(posesc-1)+1)
            cooesc(2) = zr(jcoor-1+3*(posesc-1)+2)
            cooesc(3) = zr(jcoor-1+3*(posesc-1)+3)
            zr(jcoore-1+3*(idxnoe-1)+1) = cooesc(1) - coomai(1)
            zr(jcoore-1+3*(idxnoe-1)+2) = cooesc(2) - coomai(2)
            zr(jcoore-1+3*(idxnoe-1)+3) = cooesc(3) - coomai(3)
            norme=zr(jcoore-1+3*(idxnoe-1)+1)*zr(jcoore-1+3*(idxnoe-1)&
            +1)+ zr(jcoore-1+3*(idxnoe-1)+2)*zr(jcoore-1+3*(idxnoe-1)+&
            2)+ zr(jcoore-1+3*(idxnoe-1)+3)*zr(jcoore-1+3*(idxnoe-1)+&
            3)
            if (norme .ne. 0.0d0) then
                norme = sqrt(norme)
            endif
            lc = lc + norme
        enddo
        lc = lc / nbnoeu
        if (niv .eq. 2) then
            write(ifm,*) 'LC : ',lc
        endif
        lcsqua = lc * lc
!
!       BOUCLE SUR LES NOEUDS ESCLAVES POUR CALCULER W
!       -------------------------------------------------------
        call wkvect('&&CARBE3.W', 'V V R', nbdles*nbdles, jw)
        call getvr8(motfac, 'COEF_ESCL', idxrbe, iarg, nbnoeu,&
                    zr(jcofes), nbcfes)
        if (nbcfes .lt. 0) then
            nbcfes = -nbcfes
        endif
!
        if (nbddl .ne. 1 .and. nbcfes .ne. 1 .and. nbddl .ne. nbcfes) then
            vali(1) = nbddl
            vali(2) = nbcfes
            call u2mesi('F', 'MODELISA10_12', 2, vali)
        endif
!
        if (nbcfes .ne. 1 .and. nbddl .ne. nbnoeu) then
            vali(1) = nbddl
            vali(2) = nbnoeu
            call u2mesi('F', 'MODELISA10_13', 2, vali)
        endif
!
        nbcol = 0
        inilig = 0
        do idxnoe = 1, nbnoeu
            if (nbcfes .eq. 1) then
                cofesc = zr(jcofes-1+1)
            else
                cofesc = zr(jcofes-1+idxnoe)
            endif
!
            frstco = .true.
            cntlig = 0
            do idxcol = 1, 6
                if (zl(jcescl-1+(idxnoe-1)*6+idxcol)) then
                    nbcol = nbcol + 1
                    nblign = inilig
                    do idxlig = 1, 6
                        if (zl(jcescl-1+(idxnoe-1)*6+idxlig)) then
                            nblign = nblign + 1
                            if (frstco) then
                                cntlig = cntlig + 1
                            endif
                            idxvec = nbdles*(nbcol-1)+nblign
                            if (idxlig .ne. idxcol) then
                                zr(jw-1+idxvec) = 0.0d0
                            else if (idxcol.le.3) then
                                zr(jw-1+idxvec) = cofesc
                            else
                                zr(jw-1+idxvec) = cofesc * lcsqua
                            endif
                        endif
                     enddo
                    frstco = .false.
                endif
             enddo
            inilig = inilig + cntlig
        enddo
!
        if (niv .eq. 2) then
            write (ifm,*) 'IMPRESSION W'
            do idxlig = 1, nbdles
                write (ifm,*) 'LIGNE : ',idxlig, (zr(jw-1+idxlig+&
                nbdles*(idxcol-1)),idxcol=1,nbdles)
            enddo
        endif
!
!       BOUCLE SUR LES NOEUDS ESCLAVES POUR CALCULER S
!       -------------------------------------------------------
        nblign = 0
        inilig = 0
        call wkvect('&&CARBE3.S', 'V V R', nbdles*6, js)
        do idxnoe = 1, nbnoeu
            frstco = .true.
            cntlig = 0
            do idxcol = 1, 6
                nblign = inilig
                do idxlig = 1, 6
                    if (zl(jcescl-1+(idxnoe-1)*6+idxlig)) then
                        nblign = nblign + 1
                        if (frstco) then
                            cntlig = cntlig + 1
                        endif
                        idxvec = nbdles*(idxcol-1)+nblign
                        if ((idxlig.le.3 .and. idxcol.le.3) .or.&
                            (idxlig.ge.4 .and. idxcol.ge.4)) then
                            if (idxlig .eq. idxcol) then
                                zr(js-1+idxvec) = 1.0d0
                            else
                                zr(js-1+idxvec) = 0.0d0
                            endif
                        else if (idxlig.ge.4 .and. idxcol.le.3) then
                            zr(js-1+idxvec) = 0.0d0
                        else
                            if (idxlig .eq. 1 .and. idxcol .eq. 5) then
                                zr(js-1+idxvec) = zr(jcoore-1+3*( idxnoe-1)+3)
                                else if (idxlig.eq.1 .and. idxcol.eq.6)&
                            then
                                zr(js-1+idxvec) = -zr(jcoore-1+3*( idxnoe-1)+2)
                                else if (idxlig.eq.2 .and. idxcol.eq.4)&
                            then
                                zr(js-1+idxvec) = -zr(jcoore-1+3*( idxnoe-1)+3)
                                else if (idxlig.eq.2 .and. idxcol.eq.6)&
                            then
                                zr(js-1+idxvec) = zr(jcoore-1+3*( idxnoe-1)+1)
                                else if (idxlig.eq.3 .and. idxcol.eq.4)&
                            then
                                zr(js-1+idxvec) = zr(jcoore-1+3*( idxnoe-1)+2)
                                else if (idxlig.eq.3 .and. idxcol.eq.5)&
                            then
                                zr(js-1+idxvec) = -zr(jcoore-1+3*( idxnoe-1)+1)
                            else
                                zr(js-1+idxvec) = 0.0d0
                            endif
                        endif
                    endif
                 enddo
                frstco = .false.
             enddo
            inilig = inilig + cntlig
        enddo
!
        if (niv .eq. 2) then
            write (ifm,*) 'IMPRESSION S'
            do idxlig = 1, nbdles
                write (ifm,*) 'LIGNE : ',idxlig, (zr(js-1+idxlig+&
                nbdles*(idxcol-1)),idxcol=1,6)
            enddo
        endif
!
        call wkvect('&&CARBE3.XAB', 'V V R', nbdles*6, jxab)
        call utbtab('ZERO', nbdles, 6, zr(jw), zr(js),&
                    zr(jxab), stws)
!
        if (niv .eq. 2) then
            write (ifm,*) 'IMPRESSION MATRICE MGAUSS'
            do idxlig = 1, 6
                write (ifm,*) 'LIGNE : ', idxlig, (stws(idxcol,&
                idxlig),idxcol=1,6)
            enddo
        endif
!
        do idxlig = 1, 6
            do idxcol = 1, 6
                if (idxcol .eq. idxlig) then
                    x(idxcol, idxlig) = 1.0d0
                else
                    x(idxcol, idxlig) = 0.0d0
                endif
            enddo
        enddo
!
        call mgauss('NFSP', stws, x, 6, 6,&
                    6, rbid, iret)
!
        if (iret .ne. 0) ASSERT(.false.)
!
        if (niv .eq. 2) then
            write (ifm,*) 'IMPRESSION MATRICE X'
            do idxlig = 1, 6
                write (ifm,*) 'LIGNE : ',idxlig, (x(idxcol, idxlig),&
                idxcol=1,6)
            enddo
        endif
!
        call pmppr(zr(js), nbdles, 6, -1, zr(jw),&
                   nbdles, nbdles, -1, zr(jxab), 6,&
                   nbdles)
!
        call jedetr('&&CARBE3.W')
        call jedetr('&&CARBE3.S')
!
        call wkvect('&&CARBE3.B', 'V V R', nbdles*6, jb)
!
        call pmppr(x, 6, 6, -1, zr(jxab),&
                   6, nbdles, 1, zr(jb), 6,&
                   nbdles)
!
        call jedetr('&&CARBE3.XAB')
!
! --- ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! --- APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! --- ASSEMBLEE :
! --- SI OUI TYPLAG = '22'
! --- SI NON TYPLAG = '12'
!
        call getvtx(motfac, 'NUME_LAGR', idxrbe, iarg, 0,&
                    k8bid, nbent)
!
        if (nbent .ne. 0) then
            call getvtx(motfac, 'NUME_LAGR', idxrbe, iarg, 1,&
                        numlag, nbent)
            if (numlag(1:5) .eq. 'APRES') then
                typlag = '22'
            else
                typlag = '12'
            endif
        else
            typlag = '12'
        endif
!
        do idxlig = 1, 6
            if (ddlmai(idxlig)) then
                idxter = 1
                zk8(jnznor-1+idxter) = noemai
                zr(jcmur-1+idxter) = -1.0d0
                zk8(jnzddl-1+idxter) = ddltrr(idxlig)
                idxddl = 1
                do idxcol = 1, nbdles
                    idxvec = 6*(idxcol-1)+idxlig
                    if (zr(jb-1+idxvec) .ne. 0) then
                        idxter = idxter + 1
                        zk8(jnzddl-1+idxter) = zk8(jddl-1+idxddl)
                        zk8(jnznor-1+idxter) = zk8(jnorel-1+idxddl)
                        zr(jcmur-1+idxter) = zr(jb-1+idxvec)
                    endif
                    idxddl = idxddl + 1
                enddo
                call afrela(zr(jcmur), zc(jcmuc), zk8(jnzddl), zk8( jnznor), zi(jdime),&
                            zr(jdirec), idxter, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
            endif
        enddo
!
        call jedetr('&&CARBE3.B')
!
    end do
!
!     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ---------------------------------------------
    call aflrch(lisrel, charge)
!
    call jedema()
!
!     IF (NIV.EQ.2) THEN
!       CALL JXVERI(' ')
!     ENDIF
!
999 continue
!
end subroutine
