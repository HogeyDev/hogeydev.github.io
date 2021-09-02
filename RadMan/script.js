const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

class Player {
	constructor() {
		this.color = 'black';
		this.radius = 15;
		this.moveSpeed = 3;
		this.x = 0;
		this.y = 0;
	}
	move(dir) {
		switch(parseInt(dir)) {
			case 0:
				this.x -= this.moveSpeed;
				break;
			case 1:
				this.y -= this.moveSpeed;
				break;
			case 2:
				this.x += this.moveSpeed;
				break;
			case 3:
				this.y += this.moveSpeed;
				break;
		}
	}
	draw() {
		ctx.beginPath();
		ctx.arc(canvas.width / 2, canvas.height / 2, this.radius, 0, 2 * Math.PI,  false);
		ctx.fillStyle = this.color;
		ctx.fill();
		ctx.closePath();
	}
}
class Map {
	constructor() {

	}
	draw(player, image) {
		this.x = -player.x - image.width / 2;
		this.y = -player.y - image.height / 2;
		// this.x = -player.x - 500;
		// this.y = -player.y - 500;
		ctx.drawImage(image, this.x, this.y);
	}
}
let reshoot = false;
function render() {
	frames++
	for (let i = 0; i < move.length - 1; i++) {
		if (move[i]) {
			player.move(i);
		}
	}
	if (frames % 10 == 0) {reshoot = true}
	if (move[4]) {bullets.push(new Bullet()); reshoot = false}
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	bg.draw(player, bgimage);
	for (let i = 0; i < bullets.length; i++) {
		bullets[i].draw({'x': player.x, 'y': player.y});
		if (distance(bullets[i].x, bullets[i].y, bullets[i].orig[0], bullets[i].orig[1]) > bullets[i].distance) {
			bullets.splice(i, 1)
		}
	}
	player.draw();
	ctx.font = '48px sans';
	ctx.fillText(player.x + ', ' + player.y, 20, 50);
}
class Gun {
	constructor() {
		// this.x 
	}
}
class Bullet {
	constructor(speed, distance)  {
		this.color = '#AF9B60';
		this.speed = speed || 7;
		this.x = player.x;
		this.y = player.y;
		this.orig = [player.x, player.y];
		this.distance = distance || 2000;
		this.angle = Math.atan2(-mouse.y, -mouse.x);
		this.velocity = {'x': Math.cos(this.angle) * -this.speed, 'y': Math.sin(this.angle) * -this.speed}
	}
	draw(gun) {
		this.x += this.velocity.x;
		this.y += this.velocity.y;
		this.relx = this.x - gun.x + canvas.width / 2;
		this.rely = this.y - gun.y + canvas.height / 2;
		ctx.fillStyle = this.color;
		ctx.fillRect(this.relx, this.rely, 10, 10);
	}
}
class Enemy {

}

var player = new Player();
var move = [false, false, false, false, false];
const bgimage = document.getElementById('bg');
var bg = new Map();
var mouse = {};
var bullets = [];
var frames = 0;

function distance(x1, y1, x2, y2) {
	return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
}
document.onkeydown = function(e) {
	if (e.key == 'a') {move[0] = true}
	if (e.key == 'w') {move[1] = true}
	if (e.key == 'd') {move[2] = true}
	if (e.key == 's') {move[3] = true}
}
document.onkeyup = function(e) {
	if (e.key == 'a') {move[0] = false}
	if (e.key == 'w') {move[1] = false}
	if (e.key == 'd') {move[2] = false}
	if (e.key == 's') {move[3] = false}
}
setInterval(render, 10);
document.onmousemove = function(e) {
	mouse = {'x': e.clientX - canvas.width / 2, 'y': e.clientY - canvas.height / 2};
}
document.onmousedown = function(e) {
	move[4] = true;
}

document.onmouseup = function(e) {
	move[4] = false;
}