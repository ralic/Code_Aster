subroutine rctrac(jmat, ktrac, nomcl, temp, jprol,&
                  jvale, nbvale, e, materi)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!     DETERMINATION DU MODULE DE YOUNG ET DE LA FONCTION D'ECROUISSAGE
!     A PARTIR DE LA COURBE DE TRACTION D'UN MATERIAU DONNE
!
! --------------------------------------------------------------------------------------------------
!
! in  jmat      : adresse du matériau code
! in  ktrac     :   1 -> 'traction'
!                   2 -> 'meta_traction'
!                   3 -> 'meta_trac_zirc'
! in  nomcl     : nom du mot clef pour la courbe de traction
!                   si ktrac = 1    nomcl = 'sigm'
!                   si ktrac = 2    nomcl = 'sigm_f' ou 'sigm_b' ou 'sigm_m' ou 'sigm_a'
! in  temp      : température au point de gauss considéré
! out jprol     : adresse de l'objet .prol de la s.d. fonction r(p)
! out jvale     : adresse de l'objet .vale de la s.d. fonction r(p)
! out nbvale    : nombre de valeurs de la fonction r(p)
! out e         : module de young
! in  materi    : nom du matériau
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
    integer :: jprol, jvale, nbvale, jmat
    real(kind=8) :: temp, e
    integer :: ktrac
    character(len=*) :: nomcl
    character(len=*), optional, intent(in) :: materi
!
! --------------------------------------------------------------------------------------------------
!
    integer :: imate, nbmat
    integer :: icomp, ipi, idf, nbf, ivalk, ik, ipif, ipifc, jpro, kmat
    integer :: jvalf1, nbvf1, kk, k1, k2, nar, inom
    integer :: jvaln, nbvn, ii, jj, jvalf2, nbvf2
    real(kind=8) :: coef, tole, t1, t2, e1, e2, z1, z2, zp1, zp2
    real(kind=8) :: rprim1, rp1, rprim2, rp2
    aster_logical :: procon, ltrac
    character(len=1)  :: pro1, pro2
    character(len=8)  :: nomi
    character(len=24) :: valk(2)
!
! --------------------------------------------------------------------------------------------------
!   PARAMETER ASSOCIE AU MATERIAU CODE
    integer :: lmat, lfct, lsup
    parameter  ( lmat = 9 , lfct = 10, lsup=2)
!
! --------------------------------------------------------------------------------------------------
!   TOLE=R8PREM() TROP PETIT
    tole=1.d-6
!
    ipi= 0; ipif= 0; ii= 0; imate= 0
    e1= 0.0d0; e2=0.0d0; z1= 0.0d0; z2= 0.0d0; zp1= 0.0d0; zp2= 0.0d0
    procon=.false.
!
    nbmat=zi(jmat)
    if ( nbmat.ne.1 ) then
        ASSERT( present(materi) )
        do kmat = 1, nbmat
            inom=zi(jmat+kmat)
            nomi=zk8(inom)
            if (nomi .eq. materi) then
!               Coded material
                imate = jmat+zi(jmat+nbmat+kmat)
                goto 5
            endif
        enddo
        call utmess('F', 'CALCUL_45', sk=materi)
    else
!       Coded material
        imate = jmat+zi(jmat+nbmat+1)
    endif
5   continue
!
!   courbe de traction sans métallurgie
    if (ktrac .eq. 1) then
        do  icomp = 1, zi(imate+1)
            if ('TRACTION' .eq. zk32(zi(imate)+icomp-1)(1:8)) then
                ipi=zi(imate+2+icomp-1)
                goto 20
            endif
        enddo
        call utmess('F', 'MODELISA6_80')
 20     continue
        idf=zi(ipi)+zi(ipi+1)
        nbf=zi(ipi+2)
        ivalk=zi(ipi+3)
        do ik = 1, nbf
            if ('SIGM    ' .eq. zk16(ivalk+idf+ik-1)) then
                ipif=ipi+lmat-1+lfct*(ik-1)
                goto 40
            endif
        enddo
        ASSERT(.false.)
 40     continue
    endif
!
!   courbe de traction avec métallurgie
    if (ktrac .eq. 2) then
        do icomp = 1, zi(imate+1)
            if ('META_TRACTION' .eq. zk32(zi(imate)+icomp-1)(1:13)) then
                ipi=zi(imate+2+icomp-1)
                goto 60
            endif
        enddo
        call utmess('F', 'MODELISA6_82')
 60     continue
!
        idf=zi(ipi)+zi(ipi+1)
        nbf=zi(ipi+2)
        ivalk=zi(ipi+3)
!
        if (nomcl(1:7) .eq. 'SIGM_F1') then
            do ik = 1, nbf
                if ('SIGM_F1  ' .eq. zk16(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 80
                endif
            enddo
            call utmess('F', 'MODELISA6_83')
 80         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F2') then
            do ik = 1, nbf
                if ('SIGM_F2  ' .eq. zk16(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 100
                endif
            enddo
            call utmess('F', 'MODELISA6_84')
100         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F3') then
            do ik = 1, nbf
                if ('SIGM_F3  ' .eq. zk16(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 120
                endif
            enddo
            call utmess('F', 'MODELISA6_85')
120         continue
        endif
!
        if (nomcl(1:7) .eq. 'SIGM_F4') then
            do ik = 1, nbf
                if ('SIGM_F4  ' .eq. zk16(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 140
                endif
            enddo
            call utmess('F', 'MODELISA6_86')
140         continue
        endif
!
        if (nomcl(1:6) .eq. 'SIGM_C') then
            do ik = 1, nbf
                if ('SIGM_C  ' .eq. zk16(ivalk+idf+ik-1)) then
                    ipif=ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
                    goto 160
                endif
            enddo
            call utmess('F', 'MODELISA6_87')
160         continue
        endif
    endif
!
    ipifc=zi(ipif+6)
    jprol=zi(ipifc)
    jvale=zi(ipifc+1)
!
!   LES POINTEURS JPROL ET JVALE SUR ZONE DE TRAVAIL RDPE  ETENDU SONT MODIFIES PAR RCTRAC
    jpro=zi(ipif+1)
!
    if (zk24(jpro)(1:1) .eq. 'C') then
!       FONCTION CONSTANTE - IMPOSSIBLE
        call utmess('F', 'MODELISA6_88', sk=nomcl)
    else if (zk24(jpro)(1:1).eq.'F') then
!       FONCTION D'UNE SEULE VARIABLE
        jvalf1=zi(ipif+2)
        nbvf1=zi(ipif)
        nbvale=nbvf1
!
        do kk = 1, nbvf1
            zr(jvale+kk-1)=zr(jvalf1+kk-1)
            zr(jvale+kk-1+nbvf1)=zr(jvalf1+kk-1+nbvf1)
        enddo
        zk24(jprol+4)=zk24(jpro+4)
        zk24(jprol+5)=zk24(jpro+5)
!
    else if (zk24(jpro)(1:1).eq.'N') then
!       NAPPE : FONCTION DE DEUX VARIABLES, DETERMINATION DE ii TEL QUE
!       ZR(JVALN+ii) < TEMP < ZR(JVALN+ii+1)
        jvaln=zi(ipif+4)
        nbvn=zi(ipif+5)
        zk24(jprol+5)=zk24(jpro+5)
        if (temp .lt. zr(jvaln)) then
            if (zk24(jpro+4)(1:1) .eq. 'C') then
                ii=1
                procon=.true.
            else if (zk24(jpro+4)(1:1).eq.'L') then
                ii=1
            else if (zk24(jpro+4)(1:1).eq.'E') then
                call utmess('F', 'MODELISA6_89', sk=nomcl)
            endif
        else if (temp.gt.zr(jvaln+nbvn-1)) then
            if (zk24(jpro+4)(2:2) .eq. 'C') then
                ii=nbvn
                procon=.true.
            else if (zk24(jpro+4)(2:2).eq.'L') then
                ii=nbvn-1
            else if (zk24(jpro+4)(2:2).eq.'E') then
                call utmess('F', 'MODELISA6_90', sk=nomcl)
            endif
        else
            do jj = 1, nbvn-1
!               ZR(JVALN+jj-1) < TEMP < ZR(JVALN+jj)
                t1=zr(jvaln+jj-1)
                t2=zr(jvaln+jj)
                if (abs(t1-temp) .le. (tole*abs(t1))) then
                    ii=jj
                    procon=.true.
                    goto 190
                endif
                if (abs(t2-temp) .le. (tole*abs(t2))) then
                    ii=jj+1
                    procon=.true.
                    goto 190
                endif
                if (t1 .lt. temp .and. temp .lt. t2) then
                    ii=jj
                    goto 190
                endif
            enddo
190         continue
        endif
!
!       INTERPOLATION ENTRE ii ET ii+1
        jvalf1=zi(ipif+2)+zi(zi(ipif+3)+ii-1)-1
        nbvf1=zi(zi(ipif+3)+ii)-zi(zi(ipif+3)+ii-1)
        nbvf1=nbvf1/2
        if (procon) then
!           SI LE PROLONGEMENT EST CONSTANT, ON SE RAMENE AU CAS FONCTION
            do kk = 1, nbvf1
                zr(jvale+kk-1)=zr(jvalf1+kk-1)
                zr(jvale+kk-1+nbvf1)=zr(jvalf1+kk-1+nbvf1)
            enddo
            zk24(jprol+4)=zk24(jpro+6+2*ii)
            nbvf2=nbvf1
            nbvale=nbvf1
        else
!           INTERPOLATION POUR LA FONCTION ENTRE ii ET ii+1
            zk24(jprol+4)(1:2)='CC'
            if (zk24(jpro+6+2*ii)(1:1) .eq. 'E' .or. zk24(jpro+6+2*ii+2)( 1:1) .eq. 'E') then
                zk24(jprol+4)(1:1)='E'
            else if (zk24(jpro+6+2*ii)(1:1).eq.'L' .or. zk24(jpro+6+2*ii+2)(1:1).eq.'L') then
                zk24(jprol+4)(1:1)='L'
            endif
            if (zk24(jpro+6+2*ii)(2:2) .eq. 'E' .or. zk24(jpro+6+2*ii+2)( 2:2) .eq. 'E') then
                zk24(jprol+4)(2:2)='E'
            else if (zk24(jpro+6+2*ii)(2:2).eq.'L' .or. zk24(jpro+6+2*ii+2)(2:2).eq.'L') then
                zk24(jprol+4)(2:2)='L'
            endif
            jvalf2=zi(ipif+2)+zi(zi(ipif+3)+ii)-1
            nbvf2=zi(zi(ipif+3)+ii+1)-zi(zi(ipif+3)+ii)
            nbvf2=nbvf2/2
!           INTERPOLATION ENTRE LES COURBES R(P,T1) ET R(P,T2)
!           INITIALISATION DES VARIABLES  ET INTERPOLATION POUR P=0
            nbvale=nbvf1+nbvf2-1
            coef=(temp-zr(jvaln+ii-1))/(zr(jvaln+ii)-zr(jvaln+ii-1))
            ltrac=.false.
            if (ktrac .eq. 1) then
                e1=zr(jvalf1+nbvf1)/zr(jvalf1)
                e2=zr(jvalf2+nbvf2)/zr(jvalf2)
                e=e1+coef*(e2-e1)
                ltrac=.true.
                zr(jvale)=0.d0
                k1=1
                k2=1
            else
                nbvale=nbvale+1
                z1=zr(jvalf1)
                z2=zr(jvalf2)
                if (abs(z2-z1) .le. (tole*z1)) then
                    zr(jvale)=z1
                    k1=1
                    k2=1
                else if (z2.gt.z1) then
                    zr(jvale)=z1
                    k1=1
                    k2=0
                else
                    zr(jvale)=z2
                    k1=0
                    k2=1
                endif
            endif
!
            zr(jvale+nbvale)=zr(jvalf1+nbvf1)+ coef*(zr(jvalf2+nbvf2)-zr(jvalf1+nbvf1))
            pro1=zk24(jpro+6+2*ii)(2:2)
            pro2=zk24(jpro+6+2*ii+2)(2:2)
            nar=0
!
!           DEBUT DE LA BOUCLE D'INTERPOLATION
!               LA LONGUEUR DE CETTE BOUCLE VAUT NBVF1+NBVF2-2
!               DANS LE <<CAS COURANT>>:
!                   ON PROGRESSE POINT PAR POINT PAR ABSCISSES CROISSANTES,
!                   AVEC 2 COMPTEURS K1 ET K2 (UN PAR COURBE)
!                   LE CPTEUR NAR (NBRE A RETIRER) EST INCREMENTE DE 1 A CHAQUE
!                   FOIS QUE 2 POINTS DES COURBES 1 ET 2 ONT LA MEME ABSCISSE
            do kk = 1, nbvale-1
                if ((k1.lt.nbvf1) .or. (k2.lt.nbvf2)) then
!                   CAS OU ON ARRIVE AU BOUT DE LA COURBE 1 :
!                       PROLONGEMENT EXCLUS => ON CALCULE NAR ET ON ARRETE
!                       SINON (L OU C) =>ON FAIT L'INTERPOLATION A L'ABSCISSE Z2 ET ON INCREMENTE K2
                    if (k1 .ge. nbvf1) then
                        if (pro1 .eq. 'E') then
                            nar=nbvale-kk
                            goto 230
                        else if (pro1.eq.'L') then
                            rprim1=(zr(jvalf1+nbvf1+k1-1)-zr(jvalf1+nbvf1+k1-2))/ (z1-zp1)
                        else
                            rprim1=0.d0
                        endif
                        z2=zr(jvalf2+k2)
                        if (ltrac) z2=z2-zr(jvalf2+nbvf2+k2)/e2
                        zr(jvale+kk)=z2
                        rp1=zr(jvalf1+nbvf1+k1-1)+rprim1*(z2-z1)
                        rp2=zr(jvalf2+nbvf2+k2)
                        zr(jvale+nbvale+kk)=rp1+coef*(rp2-rp1)
                        k2=k2+1
                        goto 210
                    else
                        zp1=zr(jvalf1+k1-1)
                        if (ltrac) zp1=zp1-zr(jvalf1+nbvf1+k1-1)/e1
                        z1=zr(jvalf1+k1)
                        if (ltrac) z1=z1-zr(jvalf1+nbvf1+k1)/e1
                    endif
!
!                   CAS OU ON ARRIVE AU BOUT DE LA COURBE 2 : IDEM
                    if (k2 .ge. nbvf2) then
                        if (pro2 .eq. 'E') then
                            nar=nbvale-kk
                            goto 230
                        else if (pro2.eq.'L') then
                            rprim2=(zr(jvalf2+nbvf2+k2-1)-zr(jvalf2+nbvf2+k2-2))/ (z2-zp2)
                        else
                            rprim2=0.d0
                        endif
                        z1=zr(jvalf1+k1)
                        if (ltrac) z1=z1-zr(jvalf1+nbvf1+k1)/e1
                        zr(jvale+kk)=z1
                        rp1=zr(jvalf1+nbvf1+k1)
                        rp2=zr(jvalf2+nbvf2+k2-1)+rprim2*(z1-z2)
                        zr(jvale+nbvale+kk)=rp1+coef*(rp2-rp1)
                        k1=k1+1
                        goto 210
                    else
                        zp2=zr(jvalf2+k2-1)
                        if (ltrac) zp2=zp2-zr(jvalf2+nbvf2+k2-1)/e2
                        z2=zr(jvalf2+k2)
                        if (ltrac) z2=z2-zr(jvalf2+nbvf2+k2)/e2
                    endif
!                   <<CAS COURANT>> : CF COMMENTAIRE AU DEBUT DE LA BOUCLE
                    if (abs(z2-z1) .le. (tole*z1)) then
                        zr(jvale+kk)=z1
                        rp1=zr(jvalf1+nbvf1+k1)
                        rp2=zr(jvalf2+nbvf2+k2)
                        nar=nar+1
                        k1=k1+1
                        k2=k2+1
                    else
                        if (z2 .gt. z1) then
                            zr(jvale+kk)=z1
                            rp1=zr(jvalf1+nbvf1+k1)
                            rprim2=(zr(jvalf2+nbvf2+k2)-zr(jvalf2+nbvf2+k2-1))/ (z2-zp2)
                            rp2=zr(jvalf2+nbvf2+k2-1)+rprim2*(z1-zp2)
                            k1=k1+1
                        else
                            zr(jvale+kk)=z2
                            rp2=zr(jvalf2+nbvf2+k2)
                            rprim1=(zr(jvalf1+nbvf1+k1)-zr(jvalf1+nbvf1+k1-1))/ (z1-zp1)
                            rp1=zr(jvalf1+nbvf1+k1-1)+rprim1*(z2-zp1)
                            k2=k2+1
                        endif
                    endif
                    zr(jvale+nbvale+kk)=rp1+coef*(rp2-rp1)
                endif
210             continue
            enddo
!
! ------- FIN DE LA BOUCLE D'INTERPOLATION
!
230         continue
!
!           CORRECTION DE NBVALE ET DECALAGE DES ORDONNEES DE NAR VERS LA GAUCHE
            if (nar .gt. 0) then
                nbvale=nbvale-nar
                do kk = 1, nbvale
                    zr(jvale+nbvale+kk-1)=zr(jvale+nbvale+nar+kk-1)
                enddo
            endif
        endif
    else
        valk(1)=zk24(jpro)
        valk(2)=nomcl
        call utmess('F', 'MODELISA6_91', nk=2, valk=valk)
    endif
!
!   CONSTRUCTION DE LA COURBE R(P) POUR TRACTION DANS LE CAS DE LA FONCTION D'UNE SEULE VARIABLE
!   PAS LA PEINE EN METALLURGIE CAR ON DONNE DIRECTEMENT LA COURBE R(P) OU
!   PLUS EXACTEMENT LA COURBE R(R)
!
    if (ktrac .eq. 1) then
        if (zk24(jpro)(1:1) .eq. 'N') then
            if (.not.(procon)) goto 260
        else
            if (zk24(jpro)(1:1) .ne. 'F') goto 260
        endif
        e=zr(jvale+nbvale)/zr(jvale)
        zr(jvale)=0.d0
        do kk = 1, nbvale-1
            zr(jvale+kk)=zr(jvale+kk)-zr(jvale+nbvale+kk)/e
        enddo
260     continue
    endif
!
end subroutine
